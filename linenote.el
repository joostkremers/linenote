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

;; To activate the mode in a buffer, use `M-x linenote-mode`.  Then, the
;; following commands are available, which can be freely bound to keys:

;; - linenote-next-note
;; - linenote-previous-note
;; - linenote-create/open-note
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
;;         (if-let* ((note (linenote--note-at-line))
;;                   (note-path (linenote--create-note-path)))
;;           ...)
;;
;;   This occurs a few times and is actually not very pretty, because
;;   `linenote--create-note-path' calls `linenote--note-at-line'
;;   itself.  I'm not sure there's an elegant way to fix this, though.

;;; Code:

(require 'subr-x)
(require 'filenotify)
(require 'eldoc)
(require 'crm)
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

(defvar-local linenote-source-overlay nil
  "Overlay for the note being edited.")

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
if the region is inactive.  Return the overlay created.

If REMOVE is non-nil, remove any marks on the current line or region and
return nil."
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

    ;; Remove any existing note overlays at point.
    (linenote--remove-overlays-at start-pos)

    ;; We record the start and end lines of the marked text in the overlay,
    ;; in order to be able to retrieve the note file. For this reason, they
    ;; are not updated when the position of the annotated text
    ;; changes. Note that if `end-line' is nil, the note only covers one
    ;; line.
    (unless remove
      ;; If a note is put on an empty line, issue a warning, because
      ;; `{previous|next}-single-char-property-change' skip empty overlays.
      (when (= start-pos end-pos)
        (display-warning 'linenote (format "Note at empty line %d" start-line) :warning))

      (let ((ov (make-overlay start-pos end-pos)))
        (overlay-put ov 'linenote (cons start-line end-line))
        (when linenote-use-fringe
          (overlay-put ov 'before-string
                       (propertize "N" 'display (list linenote-use-fringe
                                                      'linenote--fringe-bitmap
                                                      'linenote-fringe-face))))
        (when linenote-use-highlight
          (overlay-put ov 'face 'linenote-highlight-face))
        ov))))

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
  (if-let* ((ov (linenote--note-at-line))
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

;; TODO `{next|previous}-single-char-property-change' ignore empty
;; overlays, so putting a note on an empty line is a bad idea.

(defun linenote-next-note ()
  "Move to the next note in the buffer."
  (interactive)
  (let (next-note)
    (save-excursion
      ;; If we're in a note, move out of it first.
      (when (linenote--note-at-pos)
        (goto-char (next-single-char-property-change (point) 'linenote)))
      (let ((next-note-pos (next-single-char-property-change (point) 'linenote)))
        (when (< next-note-pos (point-max))
          (setq next-note (linenote--note-at-pos next-note-pos)))))
    (if next-note
        (goto-char (overlay-start next-note))
      (user-error "No next note"))))

(defun linenote-previous-note ()
  "Move to the previous note in the buffer."
  (interactive)
  (let (prev-note)
    (save-excursion
      ;; If we're in a note, move out of it first.  We use
      ;; `linenote--note-at-line' here (unlike `linenote-next-note'),
      ;; because if point is at the end of a line, `linenote--note-at-pos'
      ;; returns nil, but we still need to detect a note on the current
      ;; line.
      (when-let* ((note (linenote--note-at-line)))
        (goto-char (overlay-start note)))
      (let ((prev-note-pos (previous-single-char-property-change (point) 'linenote)))
        (when (or (> prev-note-pos (point-min))
                  (linenote--note-at-pos (point-min)))
          (setq prev-note (linenote--note-at-pos (1- prev-note-pos))))))
    (if prev-note
        (goto-char (overlay-start prev-note))
      (user-error "No previous note"))))

(defun linenote--note-at-pos (&optional pos)
  "Return the note at POS.
POS defaults to point.  If there is no note at POS, return nil."
  (or pos
      (setq pos (point)))
  (seq-find (lambda (ov)
              (overlay-get ov 'linenote))
            (overlays-at pos)))

(defun linenote--note-at-line ()
  "Return the note overlay on the current line.
If there is no note at point, return nil."
  (seq-find (lambda (ov)
              (overlay-get ov 'linenote))
            (overlays-in (line-beginning-position) (line-end-position))))

(defun linenote--create-note-path (&optional beg end)
  "Create the file name for a note between BEG and END.
BEG and END are line numbers.  If BEG is not provided, use the note at
point, or, if there is no note at point, use the region if active, or
the current line if the region is inactive.  Return the file name as an
absolute path."
  (expand-file-name (concat (linenote--get-relpath)
                            (if beg
                                (linenote--create-linenum-string beg end)
                              (linenote--create-linenum-string-at-point))
                            "."
                            linenote-default-extension)
                    (linenote--get-note-root)))

(defun linenote-create/open-note (&optional keep-focus)
  "Open a note for the current line, creating one if none exists.
Pop up a buffer and select it, unless KEEP-FOCUS is non-nil."
  (interactive)
  (let ((buffer (find-file-noselect (linenote--create-note-path)))
        (note (linenote--mark-note)))
    (if keep-focus
        (display-buffer buffer 'reusable-frames)
      (pop-to-buffer buffer 'reusable-frames)
      (linenote-edit-mode)
      (setq-local linenote-source-overlay note))))

(defun linenote-remove-note ()
  "Remove the note at point."
  (interactive)
  (if-let* ((note (linenote--note-at-line))
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
            (linenote--mark-note (car section) (cdr section) :remove)
            (delete-window win))
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
  (dolist (ov (overlays-in pos (1+ pos)))
    (if (overlay-get ov 'linenote)
        (delete-overlay ov))))

(defun linenote--remove-all-marks ()
  "Remove all note overlays in the current buffer.
This removes both the fringe markers and the highlights."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'linenote)
      (delete-overlay ov))))

(defun linenote-relocate-note (new-start-line)
  "Relocate the note at point to NEW-START-LINE."
  (interactive "NRelocate note to line number: ")
  (when (> new-start-line (count-lines 1 (1+ (buffer-size))))
    (user-error "Line %d does not exist" new-start-line))
  (when-let* ((note (linenote--note-at-line))
              (old-lines (overlay-get note 'linenote)))
    (when (= (car old-lines) new-start-line)
      (user-error "Note is already at line %d" new-start-line))
    (let* ((old-note-path (linenote--create-note-path))
           (new-end-line (if (cdr old-lines)
                             (+ new-start-line (- (cdr old-lines) (car old-lines)))))
           (new-note-path (linenote--create-note-path new-start-line new-end-line)))
      (linenote--mark-note (car old-lines) (cdr old-lines) :remove)
      (linenote--mark-note new-start-line new-end-line)
      (rename-file old-note-path new-note-path))))

(defun linenote--adjust-note-lines (note)
  "Adjust the filename for NOTE.
NOTE is a note overlay.  Check if the lines recorded in NOTE are still
accurate and if not, adjust the lines and the associated filename."
  (let* ((recorded-lines (overlay-get note 'linenote))
         (actual-start (line-number-at-pos (overlay-start note)))
         (actual-end (line-number-at-pos (overlay-end note))))
    (when (= actual-start actual-end)
      (setq actual-end nil))
    (when (not (equal recorded-lines (cons actual-start actual-end)))
      (let ((old-note-path (linenote--create-note-path (car recorded-lines) (cdr recorded-lines)))
            (new-note-path (linenote--create-note-path actual-start actual-end)))
        (overlay-put note 'linenote (cons actual-start actual-end))
        (rename-file old-note-path new-note-path)))))

(defun linenote--adjust-all-notes ()
  "Adjust the line numbers and file names of all notes."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (if (overlay-get ov 'linenote)
        (linenote--adjust-note-lines ov))))

;; To silence the byte-compiler when we assign a value to this variable
;; directly below.
(defvar linenote-mode)

;;;###autoload
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
    (add-hook 'after-save-hook #'linenote--adjust-all-notes :local)

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
      (add-hook 'eldoc-documentation-functions #'linenote--eldoc-show-buffer :local))))

(defun linenote--disable ()
  "Disable `linenote-mode' in the current buffer."
  (remove-hook 'eldoc-documentation-functions #'linenote--eldoc-show-buffer :local)

  (remove-hook 'kill-buffer-hook #'linenote--buffer-killed :local)
  (remove-hook 'before-revert-hook #'linenote--remove-all-marks :local)
  (remove-hook 'after-save-hook #'linenote--adjust-all-notes :local)

  (linenote--remove-all-marks)
  (linenote--dealloc-fswatch))

(defvar linenote-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c") #'linenote-finish-note)
    (define-key map (kbd "C-c k") #'linenote-cancel-note)
    map)
  "Keymap for linenote-edit-mode.")

(define-minor-mode linenote-edit-mode
  "Minor mode for editing linenote notes.
This provides commands for storing and cancelling the note."
  :init-value nil :lighter nil :global nil)

(defun linenote-finish-note ()
  "Finish the current note.
Save the note if not saved, kill the buffer and delete the window."
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (kill-buffer)
  (delete-window)
  (message "Note saved"))

(defun linenote-cancel-note ()
  "Cancel the current note.
Kill the buffer, delete the window, remove the associated file and the
note's overlay in the source buffer."
  (interactive)
  (let* ((buffer (current-buffer))
         (filename (buffer-file-name)))
    (delete-overlay linenote-source-overlay)
    (set-buffer-modified-p nil)
    (delete-window)
    (kill-buffer buffer)
    (delete-file filename))
  (message "Note cancelled"))

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

(defun linenote--eldoc-show-buffer (&optional _args)
  "Linenote documentation function for Eldoc."
  (if-let* ((_ (linenote--note-at-line))
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
  "Load tags saved in the note DIRECTORY.
This overwrites the existing tags database unconditionally."
  (setq-local linenote--tags-hashmap
              (let ((tag-file
                     (expand-file-name linenote--tags-file directory)))
                (when (file-exists-p tag-file)
                  (with-temp-buffer
                    (insert-file-contents tag-file)
                    (read (current-buffer)))))))

(defun linenote--save-tags (directory)
  "Save tags to the note DIRECTORY.
This overwrites the existing tags file unconditionally."
  (let ((tag-file (expand-file-name linenote--tags-file directory))
        (hash-str (prin1-to-string linenote--tags-hashmap nil t)))
    (with-temp-file tag-file
      (insert hash-str))))

(defun linenote-add-tags ()
  "Add tags to the note on the current line."
  (interactive)
  (unless (linenote--note-at-line)
    (user-error "No note on the current line"))
  (unless linenote--tags-hashmap
    (setq-local linenote--tags-hashmap (make-hash-table :test 'equal)))
  (let* ((tag-key (linenote--create-linenum-string-at-point))
         (prev-val (gethash tag-key linenote--tags-hashmap))
         (crm-separator "[ \t]*,[ \t]*")
         (tags (completing-read-multiple "Add tags (separated by , ): " prev-val)))
    (puthash tag-key (seq-uniq (append tags prev-val)) linenote--tags-hashmap)))

(defun linenote-remove-tags ()
  "Remove tags from the note on the current line."
  (interactive)
  (unless (linenote--note-at-line)
    (user-error "No note on the current line"))
  (if-let* ((tag-key (linenote--create-linenum-string-at-point))
            (prev-val (gethash tag-key linenote--tags-hashmap))
            (crm-separator "[ \t]*,[ \t]*")
            (tags (completing-read-multiple "Tags to remove (separated by , ): " prev-val nil t)))
      (puthash tag-key (seq-difference prev-val tags) linenote--tags-hashmap)
    (user-error "No tags to remove on the current line")))

(transient-define-prefix linenote-transient ()
  ["Linenote\n"
   [" ────Notes────"
    ("l" linenote-mode :description (lambda ()
                                      (format "%s Linenote mode" (if linenote-mode
                                                                     (propertize "*" 'face '(:weight bold))
                                                                   " "))))
    ("n" "  Create/open note" linenote-create/open-note)
    ("r" "  Relocate note" linenote-relocate-note)
    ("d" "  Delete note" linenote-remove-note)]

   [" ────Move────"
    ("f" "  Next note" linenote-next-note :transient t)
    ("b" "  Previous note" linenote-previous-note :transient t)]

   [" ────Browse────"
    ("Dn" " Dired notes directory for file" linenote-open-note-dir)
    ("Ds" " Dired notes root directory" linenote-open-root-dir)]])


(provide 'linenote)
;;; linenote.el ends here
