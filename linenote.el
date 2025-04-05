;;; linenote.el --- Line-based source code notes -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jason Kim
;; Copyright (C) 2025 Joost Kremers
;;
;; Author: Jason Kim <sukbeom.kim@gmail.com>
;; Maintainer: Joost Kremers <joostkremers@fastmail.com>
;; Created: February 18, 2024
;; Modified: Dec 31, 2024
;; Version: 1.1.3
;; Keywords: tools, note, org
;; Homepage: https://github.com/seokbeomKim/org-linenote
;; Package-Requires: ((emacs "29.1") (eldoc "1.11") (transient "0.7.2.2"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Setup:

;; (require 'linenote)

;;; Commentary:

;; This package makes it possible to attach notes to specific lines in
;; source code files.  The notes are stored in separate files and are
;; displayed when point is on a line to which a note is attached.

;; To activate the mode in a buffer, use `M-X linenote-mode`.  Then, the
;; following commands are available, which can be freely bound to keys:

;; - linenote-move-forward
;; - linenote-move-backward
;; - linenote-open/create-note
;; - linenote-remove-note
;; - linenote-open-root-dir
;; - linenote-open-note-dir

;; Notes are stored in a subdirectory of the project directory, by default
;; named `.linenote`.

;; TODO
;;
;; - Properly deal with directories that are not part of a project: Should
;;   they be excluded altogether, or should there be some fallback
;;   behaviour?
;;
;; - What happens when the user wants to create a new note in a region that
;;   already has a note attached to it?
;;
;; - Consider the following pattern:
;;
;;         (if-let* ((note (linenote--get-note-at-point))
;;                   (note-path (linenote--create-note-path)))
;;           ...)
;;
;;   This occurs a few times and is actually not very pretty, because
;;   `linenote--create-note-path' calls `linenote--get-note-at-point'
;;   itself.  I'm not sure there's an elegant way to fix this, though.

;;; Code:

(require 'subr-x)
(require 'filenotify)
(require 'eldoc)
(require 'project)
(require 'transient)

(defgroup linenote nil
  "Line-based source code notes."
  :group 'tools)

(defcustom linenote-notes-directory ".linenote"
  "The name of the notes directory.
Notes are stored in a directory tree mimicking the project's directory
tree, starting this directory, which is itself created in the root
directory of the project."
  :type '(string :tag "Directory"))

(defcustom linenote-default-extension "md"
  "The default note extension."
  :type '(string :tag "Extension")
  :group 'linenote)

(defcustom linenote-use-relative t
  "If non-nil, use relative paths in tags."
  :type 'boolean
  :group 'linenote)

(defcustom linenote-use-eldoc t
  "If non-nil, use Eldoc to display the note."
  :type 'boolean
  :group 'linenote)

(defcustom linenote-use-highlight nil
  "If non-nil, highlight lines with an associated note."
  :type 'boolean
  :group 'linenote)

(defcustom linenote-use-fringe 'left-fringe
  "If non-nil, use the fringe to mark lines with a note.
The value should be `left-fringe' or `right-fringe'.  To disable the
fringe marker, set this variable to nil."
  :type '(choice (const :tag "Left fringe" left-fringe)
                 (const :tag "Right fringe" right-fringe)
                 (const :tag "No fringe markers" nil))
  :group 'linenote)

(defface linenote-highlight-face '((t (:underline (:color "#1e90ff" :position 'descent))))
  "Face used to highlight lines with notes attached to them."
  :group 'linenote)

(defface linenote-fringe-face '((t (:foreground "#1e90ff")))
  "Face for the fringe marker."
  :group 'linenote)

(defvar linenote--buffers nil
  "List of file watchers and associated buffers.")

(defvar linenote--linenum-regexp ".*#L\\([0-9]+\\)\\(?:-L\\([0-9]+\\).*\\)?"
  "Regular expression matching the line number part of a note filename.")

(defun linenote-set-fringe-bitmap (var value)
  "Setter function for `linenote-fringe-bitmap'.
VAR is the variable `linenote-fringe-bitmap', VALUE should be a list of
strings representing binary numbers.  If any string in VALUE contains
characters other than 0 or 1, an error is raised."
  (dolist (s value)
    (unless (string-match-p "\\`[01]+\\'" s)
      (error "Not a binary number: %S" s)))
  (custom-set-default var value)
  (define-fringe-bitmap 'linenote--fringe-bitmap
    (apply #'vector (mapcar (lambda (s)
                              (string-to-number s 2))
                            value))
    nil nil 'center))

(defcustom linenote-fringe-bitmap
  '("00000000"
    "00011000"
    "00111100"
    "01111110"
    "01111110"
    "00111100"
    "00011000"
    "00000000")
  "The fringe bitmap used to indicate notes.
0 means no pixel, 1 means a pixel.  In practice, this means that 1
represents the foreground color of `linenote-fringe-face' and 0
represents the background color of the fringe, or, if customized, of
`linenote-fringe-face'."
  :type '(repeat (string :tag ""))
  :group 'linenote
  :set #'linenote-set-fringe-bitmap)

(defvar-local linenote--fwatch-id nil
  "File watcher id for linenote.")

(defconst linenote--tags-file "tags")

(defvar-local linenote--tags-hashmap nil
  "A hash table for tags.")

(defun linenote--project-root ()
  "Return the current project's root.
If called outside of a project, return nil."
  (if-let ((project (project-current)))
      (expand-file-name (project-root project))))

;; TODO Check if this function should really return the empty string if
;; there is no project root.
(defun linenote--get-note-root ()
  "Get the root directory for notes in the current project.
If not in a project, return empty string.  This function uses
`project.el' under the hood."
  (if-let ((project-root (linenote--project-root)))
      (expand-file-name linenote-notes-directory project-root)
    ""))

(defun linenote--mark-note (&optional start-line end-line remove)
  "Mark the region between START-LINE and END-LINE.
Marking involves setting the fringe marker and/or highlighting the
relevant lines, depending on the user settings `linenote-use-fringe' and
`linenote-use-highlight'.

BEG and END are the start and end lines of the buffer section to be
marked.  If not provided, START-LINE and END-LINE default to the start
and end lines of the region if it is active, or to the line point is on
if the region is inactive.

If REMOVE is non-nil, remove any marks on the current line or region."
  ;; This is a little annoying: we need both the start and end lines, and
  ;; the start and end character positions.
  (let (start-pos end-pos)
    (cond
     (start-line (save-excursion  ; If a start line is provided...
                   (goto-char (point-min)) ; ...get its character position for the overlay.
                   (forward-line (1- start-line))
                   (setq start-pos (line-beginning-position))
                   (if (null end-line) ; If there's no end line...
                       (setq end-pos (line-end-position)) ; ...we mark until the end of the line.
                     (goto-char (point-min))
                     (forward-line (1- end-line))
                     (setq end-pos (line-end-position)))))
     ((use-region-p) (setq start-pos (region-beginning)
                           start-line (line-number-at-pos start-pos)
                           end-pos (region-end)
                           end-line (line-number-at-pos end-pos)))
     (t (setq start-pos (line-beginning-position)
              start-line (line-number-at-pos)
              end-pos (line-end-position)
              end-line nil))) ; nil here indicates the note only covers one line.
    (linenote--remove-overlays-at start-pos)
    ;; We record the start and end lines of the marked text in the overlay,
    ;; in order to be able to retrieve the note file. For this reason, they
    ;; are not updated when the position of the annotated text
    ;; changes. Note that if `end-line' is nil, the note only covers one
    ;; line.
    (unless remove
      (let ((ov (make-overlay start-pos end-pos)))
        (overlay-put ov 'linenote (cons start-line end-line))
        (when linenote-use-fringe
          (overlay-put ov 'before-string
                       (propertize "N" 'display (list linenote-use-fringe
                                                      'linenote--fringe-bitmap
                                                      'linenote-fringe-face))))
        (when linenote-use-highlight
          (overlay-put ov 'face 'linenote-highlight-face))))))

(defun linenote--mark-all-notes ()
  "Mark lines in the current buffer for which notes exist."
  (let* ((relpath (linenote--get-relpath))
         (notes (directory-files (expand-file-name (or (file-name-directory relpath) "")
                                                   (linenote--get-note-root))
                                 nil (file-name-base relpath))))
    (save-mark-and-excursion
      (dolist (note notes)
        (let ((lines (linenote--extract-lines-from-filename note)))
          (linenote--mark-note (car lines) (cdr lines)))))))

(defun linenote--get-relpath ()
  "Get the relative path of the current file.
The path is calculated starting from the project root.  If the current
file is not part of a project, return the filename without directory
part."
  (if-let* ((root (linenote--project-root)))
      (file-relative-name (buffer-file-name) root)
    (file-name-nondirectory (buffer-file-name))))

(defun linenote--create-linenum-string (beg &optional end)
  "Create a line number string from BEG to END."
  (if end
      (format "#L%S-L%S" beg end)
    (format "#L%S" beg)))

(defun linenote--create-linenum-string-at-point ()
  "Create a line number string for a note at point.
Return value is a string of the form \"#L<n1>-L<n2>\" or, for a single
line, \"#L<n>\".  If there is already a note at point, use its line
numbers.  Otherwise, use the start and end lines of the active
region, or just the current line if the region is inactive."
  (if-let* ((ov (linenote--get-note-at-point))
            (lines (overlay-get ov 'linenote)))
      (linenote--create-linenum-string (car lines) (cdr lines))
    (if (use-region-p)
        (linenote--create-linenum-string (line-number-at-pos (region-beginning))
                                         (line-number-at-pos (1- (region-end))))
      (linenote--create-linenum-string (line-number-at-pos)))))

(defun linenote--extract-lines-from-filename (filename)
  "Extract line range from FILENAME.
Return value is a cons of two numbers, the first and last line of
the note.  If the note only refers to a single line, the second value is
nil."
  (when (string-match linenote--linenum-regexp filename)
    (let ((beg (string-to-number (match-string 1 filename)))
          (end (and (match-beginning 2)
                    (string-to-number (match-string 2 filename)))))
      (cons beg end))))

(defun linenote--get-note-at-point ()
  "Return the note overlay at point.
If there is no note at point, return nil."
  (seq-find (lambda (ov)
              (overlay-get ov 'linenote))
            (overlays-in (line-beginning-position) (line-end-position))))

(defun linenote--create-note-path ()
  "Create the file name for a note at point.
Return the file name as an absolute path."
  (expand-file-name (concat (linenote--get-relpath)
                            (linenote--create-linenum-string-at-point)
                            "."
                            linenote-default-extension)
                    (linenote--get-note-root)))

(defun linenote-open/create-note (&optional keep-focus)
  "Open a note for the current line, creating one if none exists.
Pop up a buffer and select it, unless KEEP-FOCUS is non-nil."
  (interactive)
  (let ((buffer (find-file-noselect (linenote--create-note-path))))
    (linenote--mark-note)
    (if keep-focus
        (display-buffer buffer 'reusable-frames)
      (pop-to-buffer buffer 'reusable-frames))))

(defun linenote-remove-note ()
  "Remove the note at point."
  (interactive)
  (if-let* ((note (linenote--get-note-at-point))
            (section (overlay-get note 'linenote))
            (note-path (linenote--create-note-path))
            (buf (find-file-noselect note-path))
            (win (display-buffer buf 'reusable-frames)))
      (condition-case err
          (when (yes-or-no-p (format "Remove %S?" note-path))
            (with-current-buffer buf
              (set-buffer-modified-p nil))
            (kill-buffer buf)
            (delete-file note-path)
            (linenote--mark-note (car section) (cdr section) :remove))
        (delete-window win)
        (quit (delete-window win))
        (error (signal (car err) (cdr err))))
    (error "No note at point")))

(defun linenote--directory-files ()
  "Do `directory-files' to find notes (except for files with names ending with ~)."
  (directory-files (expand-file-name (or (file-name-directory (linenote--get-relpath)) "")
                                     (linenote--get-note-root))
                   'full (concat (file-name-base (linenote--get-relpath)) ".[^.].*[^~]$")))

(defun linenote--is-lock-file (file)
  "Check if FILE is a lock file."
  (string= (substring (file-name-base file) 0 2) ".#"))

(defun linenote--file-changed (event)
  "A function to handle file watch EVENT."
  (let* ((fs-id (nth 0 event))
         (etype (nth 1 event))
         (fpath (nth 2 event))
         (lines (linenote--extract-lines-from-filename fpath))
         (event-buffer (cdr (assoc fs-id linenote--buffers))))

    (when (and (string-match-p
                (regexp-quote (file-name-nondirectory
                               (buffer-file-name event-buffer)))
                (file-name-base fpath))
               (not (linenote--is-lock-file fpath)))
      (with-current-buffer event-buffer
        (cond
         ((string= etype "deleted")
          (linenote--mark-note (car lines) (cdr lines) t))
         ((string= etype "created")
          (linenote--mark-note (car lines) (cdr lines))))))))

(defun linenote--dealloc-fswatch ()
  "Remove out the file watchers and corresponding list."
  (file-notify-rm-watch linenote--fwatch-id)
  (setq-local linenote--overlays nil)
  (setq linenote--buffers
        (delete (assoc linenote--fwatch-id linenote--buffers) linenote--buffers)))

(defun linenote--buffer-killed ()
  "A hook function for `kill-buffer-hook'."
  (linenote--dealloc-fswatch))

(defun linenote--remove-overlays-at (pos)
  "Remove linenote overlays at character position POS."
  ;; We use `overlay-in' here, because fringe markers are created with
  ;; empty overlays, which `overlays-at' does not detect.
  (dolist (ov (overlays-in pos (1+ pos)))
    (if (overlay-get ov 'linenote)
        (delete-overlay ov))))

(defun linenote--remove-all-marks ()
  "Remove all overlays in the current buffer.
This removes both the fringe markers and the highlights."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'linenote)
      (delete-overlay ov))))

;; To silence the byte-compiler when we assign this variable directly
;; below.
(defvar linenote-mode)

(define-minor-mode linenote-mode
  "Toggle `linenote-mode'."
  :init-value nil
  :global nil
  :lighter " Linenote"
  (if linenote-mode
      (linenote--enable)
    (linenote--disable)))

(defun linenote--enable ()
  "Enable `linenote-mode' in the current buffer."
  ;; First check if we're in a project and visiting a file.
  (when (not (linenote--project-root))
    (setq linenote-mode nil)
    (error "The working directory is not a known project"))
  (when (not (buffer-file-name))
    (setq linenote-mode nil)
    (error "Not file-visiting buffer"))

  ;; Make sure the note directory exists.
  (let ((note-dir (expand-file-name
                   (or (file-name-directory (linenote--get-relpath)) "")
                   (linenote--get-note-root))))
    (make-directory note-dir t)

    ;; Set up some hooks.
    (add-hook 'kill-buffer-hook #'linenote--buffer-killed :local)
    (add-hook 'before-revert-hook #'linenote--remove-all-marks :local)

    ;; Set up a file watcher for the note directory.
    (let* ((buffer-id (current-buffer))
           ;; TODO `file-notify-add-watch' triggers an error if the file
           ;; cannot be watched. We should probably handle this.

           ;; TODO This sets up multiple file watchers for the same node
           ;; directory if we annotate more than one file in the same
           ;; source directory. We should probably handle that better.
           (watch-id (file-notify-add-watch note-dir
                                            '(change)
                                            #'linenote--file-changed)))
      (setq-local linenote--fwatch-id watch-id)
      (setq-local linenote--follow-cursor nil)
      (push `(,watch-id . ,buffer-id) linenote--buffers))

    ;; Mark all existing notes.
    (linenote--mark-all-notes)

    ;; Set up Eldoc.
    (when linenote-use-eldoc
      (setq-local eldoc-documentation-functions
                  (cons 'linenote--eldoc-show-buffer eldoc-documentation-functions)))))

(defun linenote--disable ()
  "Disable `linenote-mode' in the current buffer."
  (setq-local eldoc-documentation-functions
              (delete 'linenote--eldoc-show-buffer eldoc-documentation-functions))

  (remove-hook 'kill-buffer-hook #'linenote--buffer-killed :local)
  (remove-hook 'before-revert-hook #'linenote--remove-all-marks :local)

  (linenote--remove-all-marks)
  (linenote--dealloc-fswatch))

(defun linenote-open-root-dir ()
  "Open the linenote root directory for the current project."
  (interactive)
  (let ((note-dir (linenote--get-note-root)))
    (if (file-exists-p note-dir)
        (find-file note-dir)
      (error "No notes found"))))

(defun linenote-open-note-dir ()
  "Open the note directory for the current file."
  (interactive)
  (let ((note-dir (expand-file-name (or (file-name-directory (linenote--get-relpath)) "")
                                    (linenote--get-note-root))))
    (if (file-exists-p note-dir)
        (find-file note-dir)
      (error "No notes found"))))

(defun linenote--obtain-tag-string-by-key (key)
  "Get a tag string by the KEY from the hash table."
  (let ((result ""))
    (mapc (lambda (v)
            (setq result (concat result (format "#%s " v))))
          (gethash (format "#L%S" (car key)) linenote--tags-hashmap))
    result))

(defun linenote--add-tags-to-notelist (notes)
  "Add tags to the list of NOTES for the current buffer."
  (mapcar (lambda (note)
            (when linenote-use-relative
              (setq note (string-replace (expand-file-name linenote-notes-directory
                                                           (linenote--project-root))
                                         "" note)))
            (format "%-100s%s" note
                    (linenote--obtain-tag-string-by-key
                     (linenote--extract-lines-from-filename note))))
          notes))

(defun linenote--eldoc-show-buffer (&optional _args)
  "Linenote documentation function for Eldoc."
  (if-let* ((note (linenote--get-note-at-point))
            (note-path (linenote--create-note-path)))
      (when (file-exists-p note-path)
        (with-temp-buffer
          (insert-file-contents note-path)
          (let* ((file-buffer (buffer-string))
                 (file-ext (file-name-extension note-path))
                 (language '(("org" . "org")
                             ("md" . "markdown"))))
            (condition-case e
                (if (fboundp 'lsp--render-string)
                    (lsp--render-string file-buffer (cdr (assoc file-ext language)))
                  ;; TODO We should come up with a more portable way to
                  ;; render the buffer.
                  file-buffer)
              (error (message "handle error: %s" e))))))))

(defun linenote--load-tags (directory)
  "Load tags saved in the note DIRECTORY."
  (setq-local linenote--tags-hashmap
              (let ((tag-file
                     (expand-file-name linenote--tags-file directory)))
                (when (file-exists-p tag-file)
                  ;; load the file
                  (with-temp-buffer
                    (insert-file-contents tag-file)
                    (read (current-buffer)))))))

(defun linenote--save-tags (directory)
  "Save tags to the note DIRECTORY."
  (let ((tag-file (expand-file-name linenote--tags-file directory))
        (hash-str (prin1-to-string linenote--tags-hashmap)))
    (with-temp-file tag-file
      (insert hash-str))))

(defun linenote-add-tags ()
  "Add tags corresponding to the current line."
  (interactive)

  (if (null (linenote--get-note-at-point))
      (message "Note does not exist on the current line.")
    (let ((reldir (expand-file-name
                   (concat (file-name-directory (linenote--get-relpath)) "")
                   (linenote--get-note-root))))
      (linenote--load-tags reldir)
      (when (null linenote--tags-hashmap)
        (setq-local linenote--tags-hashmap (make-hash-table :test 'equal)))

      (let* ((tagkey (linenote--create-linenum-string-at-point))
             (prev-val (gethash tagkey linenote--tags-hashmap))
             (tagstr (completing-read-multiple "Input tags (separated by , ): " prev-val)))
        (remhash tagkey linenote--tags-hashmap)
        (if prev-val
            (puthash tagkey (append tagstr prev-val) linenote--tags-hashmap)
          (puthash tagkey tagstr linenote--tags-hashmap))
        (linenote--save-tags reldir)))))

(defun linenote-remove-tags ()
  "Remove tags corresponding to the current line."
  (interactive)

  (let ((reldir (expand-file-name (concat (file-name-directory (linenote--get-relpath)) "")
                                  (linenote--get-note-root))))

    (linenote--load-tags reldir)
    (let* ((tagkey (linenote--create-linenum-string-at-point))
           (prev-val (gethash tagkey linenote--tags-hashmap)))

      (if (null prev-val)
          (message "No tags to remove on the current line.")
        (let* ((tagstr (completing-read-multiple "Input tags to remove (separated by , ): " prev-val)))
          (mapc (lambda (v) (setq prev-val (delete v prev-val))) tagstr)
          (remhash tagkey linenote--tags-hashmap)
          (puthash tagkey prev-val linenote--tags-hashmap)
          (linenote--save-tags reldir))))))

(transient-define-prefix linenote-transient ()
  ["Linenote\n"
   [" ────Notes────"
    ("l" linenote-mode :description (lambda ()
                                      (format "%s Linenote mode" (if linenote-mode
                                                                     (propertize "*" 'face '(:weight bold))
                                                                   " "))))
    ("o" "  Create/open note" linenote-open/create-note)
    ("d" "  Delete note" linenote-remove-note)]
   [" ────Browse────"
    ("n" "  Next note" linenote-move-forward :transient t)
    ("p" "  Previous note" linenote-move-backward :transient t)]])


(provide 'linenote)
;;; linenote.el ends here
