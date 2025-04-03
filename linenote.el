;;; linenote.el --- Line-based Source code notes -*- lexical-binding: t; -*-
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
;; Package-Requires: ((emacs "29.1") (eldoc "1.11"))

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
;; - linenote-open-note
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

;;; Code:

(require 'subr-x)
(require 'filenotify)
(require 'eldoc)
(require 'project)

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

(defun linenote--mark-note (&optional beg end remove)
  "Mark the region between BEG and END.
Marking involves setting the fringe marker and/or highlighting the
relevant lines, depending on the user settings `linenote-use-fringe' and
`linenote-use-highlight'.

BEG and END are the beginning and end of the buffer section to be
marked.  If not provided, BEG and END default to the beginning and end
of the region if it is active, or to the beginning and end of the line
point is on if the region is inactive.

If REMOVE is non-nil, remove any marks on the current line or region."
  (setq beg (or beg
                (use-region-beginning)
                (line-beginning-position)))
  (setq end (or end
                (use-region-end)
                (line-end-position)))
  (linenote--remove-overlays-at beg)
  ;; We record the start and end lines of the marked text in the
  ;; overlay. These are used to retrieve the note file. For this reason,
  ;; they are not updated when the position of the annotated text changes.
  (unless remove
    (let ((start-line (line-number-at-pos beg))
          (end-line (line-number-at-pos end)))
      (when linenote-use-fringe
        ;; The fringe marker is only put on the first line of the annotated
        ;; section, so we create an overlay from `beg' to `beg'.
        (let ((ov (make-overlay beg beg)))
          (overlay-put ov 'linenote (cons start-line end-line))
          (overlay-put ov 'before-string
                       (propertize "N" 'display (list linenote-use-fringe
                                                      'linenote--fringe-bitmap
                                                      'linenote-fringe-face)))))
      (when linenote-use-highlight
        (let ((ov (make-overlay beg end)))
          (overlay-put ov 'linenote (cons start-line end-line))
          (overlay-put ov 'face 'linenote-highlight-face))))))

(defun linenote--mark-all-notes ()
  "Mark lines in the current buffer for which notes exist."
  (let* ((note-relpath (linenote--get-relpath))
         (notes (directory-files (expand-file-name (or (file-name-directory note-relpath) "")
                                                   (linenote--get-note-root))
                                 nil (file-name-base note-relpath))))
    (save-mark-and-excursion
      (dolist (note notes)
        (apply #'linenote--mark-note (linenote--extract-lines-from-filename note))))))

(defun linenote--get-relpath ()
  "Get the relative path of the current file.
The path is calculated starting from the project root.  If the current
file is not part of a project, return the filename without directory
part."
  (if-let* ((root (linenote--project-root)))
      (file-relative-name (buffer-file-name) root)
    (file-name-nondirectory (buffer-file-name))))

(defun linenote--create-linenum-string (&optional section)
  "Create a line number string for SECTION.
SECTION is a list of two line numbers, <n1> and <n2>.  Return value is a
string of the form \"#L<n1>-L<n2>\".  <n2> can also be nil, in which
case the string returned has the form \"#L<n1>\".

If SECTION is omitted, use the beginning and end of the region if the
region is active, or point if the region is inactive."
  (cond
   (section (if (cadr section)
                (format "#L%S-L%S" (car section) (cadr section))
              (format "#L%S" (car section))))
   ((use-region-p) (format "#L%S-L%S"
                           (line-number-at-pos (region-beginning))
                           (line-number-at-pos (1- (region-end)))))
   (t (format "#L%S" (line-number-at-pos)))))

(defun linenote--extract-lines-from-filename (filename)
  "Extract line range from FILENAME.
Return value is a list of two numbers, the first and last line of
the note.  If the note only refers to a single line, the second value is
nil."
  (when (string-match ".*#L\\([0-9]+\\)\\(?:-L\\([0-9]+\\).*\\)?" filename)
    (let ((beg (string-to-number (match-string 1 filename)))
          (end (and (match-beginning 2)
                    (string-to-number (match-string 2 filename)))))
      (list beg end))))

(defun linenote--get-note-linum-by-direction (line forward)
  "Check if there is a note within the LINE.
If FORWARD is non-nil, then find the next note.  Otherwise, find
the previous note."
  (let ((res
         (cond (forward (line-number-at-pos (point-max)))
               (t 0)))
        (found nil))
    (dolist (file (linenote--directory-files))
      (let* ((range (linenote--extract-lines-from-filename file))
             (min (car range))
             (f (if forward #'< #'>)))
        (if (and (funcall f line min)
                 (funcall f min res))
            (progn
              (setq found t)
              (setq res min)))))
    (if found res)))

(defun linenote--move-forward (forward)
  "Move to the next note.
If FORWARD is nil, then move to the previous note."
  (let* ((current-line (line-number-at-pos))
         (next-line (linenote--get-note-linum-by-direction
                     current-line
                     forward))
         (f (if forward #'> #'<)))
    (if (and next-line
             (funcall f next-line current-line))
        (forward-line (- next-line current-line))
      (message "No more notes"))))

(defun linenote-move-forward ()
  "Move to the next note."
  (interactive)
  (linenote--move-forward t))

(defun linenote-move-backward ()
  "Move to the previous note."
  (interactive)
  (linenote--move-forward nil))

(defun linenote--check-line-range (line)
  "Check if there is a note within LINE."
  (let ((res nil))
    (dolist (file (linenote--directory-files))
      (let* ((range (linenote--get-line-range-by-fname file))
             (min (car range))
             (max (cadr range)))
        (if (and max
                 (<= min line)
                 (<= line max))
            (setq res file)
          (if (and (null max)
                   (= min line))
              (setq res file)))))
    res))

(defun linenote--check-note-exist ()
  "Check whether the note for current line exists.
If the note exists, return the absolute path, otherwise return nil."
  (linenote--check-line-range (line-number-at-pos)))

(defun linenote--get-note-path ()
  "Create the file path for a note at point.
Return the file path as an absolute path."
  ;; TODO check if there's some way to make sure the path separator is
  ;; added without doing so explicitly.
  (expand-file-name (file-name-with-extension (concat (linenote--get-relpath)
                                                      "/"
                                                      (linenote--create-linenum-string))
                                              linenote-default-extension)
                    (linenote--get-note-root)))

(defun linenote-open-note (&optional keep-focus)
  "Open a note for the current line, creating one if none exists.
Pop up a buffer and select it, unless KEEP-FOCUS is non-nil."
  (interactive)
  (let ((buffer (find-file-noselect (linenote--get-note-path))))
    (linenote--mark-note)
    (if keep-focus
        (display-buffer buffer 'reusable-frames)
      (pop-to-buffer buffer 'reusable-frames))))

(defun linenote-remove-note ()
  "Remove the annotation on the line."
  (interactive)
  (let ((note-path (linenote--get-note-path)))
    (if (not (file-exists-p note-path))
        (error "No notes to remove from here")
      (condition-case _
          (progn
            (pop-to-buffer (find-file-noselect note-path) 'reusable-frames)
            (let ((do-remove (yes-or-no-p (format "Remove %S?" note-path))))
              (delete-window)
              (when do-remove
                (delete-file note-path)
                (linenote--mark-note (linenote--extract-lines-from-filename (file-name-base note-path)) :remove))))
        (quit (delete-window))))))

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
         (region (linenote--extract-lines-from-filename fpath))
         (event-buffer (cdr (assoc fs-id linenote--buffers))))

    (when (and (string-match-p
                (regexp-quote (file-name-nondirectory
                               (buffer-file-name event-buffer)))
                (file-name-base fpath))
               (not (linenote--is-lock-file fpath)))
      (with-current-buffer event-buffer
        (cond
         ((string= etype "deleted")
          (linenote--mark-note (car region) (cadr region) t))
         ((string= etype "created")
          (apply #'linenote--mark-note region)))))))

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
  "Remove linenote overlays at POS."
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

(defun linenote--eldoc-show-buffer (&optional args)
  "Show the first line of a candidate note in the mini-buffer.
Optional argument ARGS Return the string for eldoc.  Since we need
only note buffer, there is no usage of ARGS at all."
  (ignore args)
  (let ((note-path (linenote--get-note-path)))
    (when (and note-path (file-exists-p note-path))
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

  (if (null (linenote--check-note-exist))
      (message "Note does not exist on the current line.")
    (let ((reldir (expand-file-name
                   (concat (file-name-directory (linenote--get-relpath)) "")
                   (linenote--get-note-root))))
      (linenote--load-tags reldir)
      (when (null linenote--tags-hashmap)
        (setq-local linenote--tags-hashmap (make-hash-table :test 'equal)))

      (let* ((tagkey (linenote--create-linenum-string))
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
    (let* ((tagkey (linenote--create-linenum-string))
           (prev-val (gethash tagkey linenote--tags-hashmap)))

      (if (null prev-val)
          (message "No tags to remove on the current line.")
        (let* ((tagstr (completing-read-multiple "Input tags to remove (separated by , ): " prev-val)))
          (mapc (lambda (v) (setq prev-val (delete v prev-val))) tagstr)
          (remhash tagkey linenote--tags-hashmap)
          (puthash tagkey prev-val linenote--tags-hashmap)
          (linenote--save-tags reldir))))))

(provide 'linenote)
;;; linenote.el ends here
