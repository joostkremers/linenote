;;; linenote.el --- A package inspired by VSCode Linenote -*- lexical-binding: t; -*-
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

;; This file provides a source for linenote that manages notes based on the line
;; number in a buffer.  The package provides some interactive functions:

;; - linenote-move-forward
;; - linenote-move-backward
;; - linenote-open-note
;; - linenote-remove-note
;; - linenote-find-root-dir
;; - linenote-find-note-dir

;; All notes are stored at $PROJECT_ROOT/.linenote directory.

;;; Code:

(require 'subr-x)
(require 'filenotify)
(require 'eldoc)
(require 'project)

(defcustom linenote-default-extension ".md"
  "The default note extension."
  :type 'string
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
  "Highlight style for the notes.")

(defface linenote-fringe-face '((t (:foreground "#1e90ff")))
  "Fringe color for the notes.")

(defvar linenote--prev-window -1
  "Temporary value to store previously focused window.")

(defvar linenote--buffers nil
  "The target buffer to ensure line tracking.")

(defun linenote-set-fringe-bitmap (var value)
  "Setter function for `linenote-fringe-bitmap'.
VAR is the variable `linenote-fringe-bitmap', VALUE should be a list of
strings representing binary numbers.  If any string in VALUE contains
characters other than 0 or 1, an error is raised."
  (mapc (lambda (s)
          (unless (string-match-p "\\`[01]+\\'" s)
            (error "Not a binary number: %S" s)))
        value)
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
  "Define a fringe bitmap to indicate notes."
  :type '(repeat (string :tag ""))
  :group 'linenote
  :set #'linenote-set-fringe-bitmap)

(defvar-local linenote--fwatch-id nil
  "File watcher id for linenote.")

(defvar-local linenote--fringe-markers nil
  "A list of fringe markers.")

(defconst linenote--tags-file "tags")

(defvar-local linenote--tags-hashmap nil
  "A hash table for tags.")

(defun linenote--project-root ()
  "Return the current project's root.
If called outside of a project, return nil."
  (if-let ((project (project-current)))
      (expand-file-name (project-root project))))

(defun linenote--mark-note (filename &optional remove)
  "Mark the line specified in FILENAME.
If REMOVE is non-nil, remove markers for the relevant lines.

Marking involves setting the fringe marker and/or highlighting the
relevant lines, depending on the user settings `linenote-use-fringe' and
`linenote-use-highlight'."
  (save-mark-and-excursion
    (let* ((lines (linenote--lines-to-highlight filename))
           (min-line (1- (car lines)))
           (max-line (1- (car (cdr lines))))
           (diff-line (- max-line min-line)))
      (goto-char (point-min))
      (forward-line min-line)
      ;; TODO We should remove all overlays in one go, not in two steps
      ;; (the fringe marker here, the highlight below). We *should*,
      ;; however, make sure we only delete our own overlays, which is not
      ;; happening here right now.
      (mapc (lambda (v) (delete-overlay v))
            (overlays-in (line-beginning-position) (line-end-position)))
      (when linenote-use-fringe
        (unless remove
          (let ((ov (make-overlay (point) (point))))
            (overlay-put ov 'before-string
                         (propertize "N" 'display (list linenote-use-fringe
                                                        'linenote--fringe-bitmap
                                                        'linenote-fringe-face)))
            (push ov linenote--fringe-markers))))
      (when linenote-use-highlight
        (beginning-of-line)
        (set-mark (line-beginning-position))
        (forward-line diff-line)
        (linenote--remove-overlays-at (region-beginning))
        (unless remove
          (let ((ov (make-overlay (region-beginning) (1- (region-end)))))
            (overlay-put ov 'face 'linenote-highlight-face)
            (if (overlay-buffer ov)
                (push ov linenote--overlays))))))))

(defun linenote-mark-notes ()
  "Mark lines in the current buffer for which notes exist."
  (let* ((note-relpath (linenote--get-relpath))
         (notes-list (directory-files (expand-file-name (or (file-name-directory note-relpath) "")
                                                        (linenote--get-note-rootdir))
                                      nil (file-name-base note-relpath))))
    (mapc #'linenote--mark-note notes-list)))

(defun linenote--get-relpath ()
  "Get the relative path of the current file."
  (if (linenote--project-root)
      (string-remove-prefix (linenote--project-root) (buffer-file-name))
    (file-name-nondirectory (buffer-file-name))))

(defun linenote--get-note-rootdir ()
  "Get the root directory of the note.
If not in a project, return empty string.  This function uses
`project.el' under the hood."
  (if-let ((project-root (linenote--project-root)))
      (let ((note-dir (expand-file-name ".linenote" project-root)))
        (unless (file-exists-p note-dir)
          (make-directory note-dir t))
        note-dir)
    ""))

(defun linenote--create-linenum-string ()
  "Create a line number string for the line at point.
Return value is a string of the form \"#L<n>\", with \"n\" being the
line number.  If the region is active, return a line range instead, of
the form \"#L<n>-L<m>\", with \"n\" being the first line of the region
and \"m\" the last line."
  (if (use-region-p)
      (format "#L%S-L%S"
              (line-number-at-pos (use-region-beginning))
              (line-number-at-pos (- (use-region-end) 1)))
    (format "#L%S" (line-number-at-pos))))

(defun linenote--get-line-range-by-fname (filename)
  "Extract line range from FILENAME.
Return value is a cons cell of two numbers, the first and last line of
the note.  If the note only refers to a single line, the second value is
nil."
  (when (string-match ".*#L\\([0-9]+\\)\\(?:-L\\([0-9]+\\).*\\)?" filename)
    (let ((min (string-to-number (match-string 1 filename)))
          (max (if (match-beginning 2)
                   (string-to-number (match-string 2 filename))
                 nil)))
      (cons min max))))

(defun linenote--get-note-linum-by-direction (line forward)
  "Check if there is a note within the LINE.
If FORWARD is non-nil, then find the next note.  Otherwise, find
the previous note."
  (let ((res
         (cond (forward (line-number-at-pos (point-max)))
               (t 0)))
        (found nil))
    (dolist (file (linenote--directory-files))
      (let* ((range (linenote--get-line-range-by-fname file))
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
             (max (cdr range)))
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

(defun linenote--get-candidate-note-path ()
  "Get the note's absolute path for corresponding line."
  (or (linenote--check-note-exist)
      (expand-file-name (concat (linenote--get-relpath)
                                (linenote--create-linenum-string)
                                linenote-default-extension)
                        (linenote--get-note-rootdir))))

(defun linenote-open-note (&optional keep-focus)
  "Open a note for the current line, creating one if none exists.
Pop up a buffer and select it, unless KEEP-FOCUS is non-nil."
  (interactive)
  (let* ((note-path (linenote--get-candidate-note-path))
         (buffer (find-file-noselect note-path)))
    (linenote--mark-note note-path)
    (if keep-focus
        (display-buffer buffer 'reusable-frames)
      (pop-to-buffer buffer 'reusable-frames))))

(defun linenote-remove-note ()
  "Remove the annotation on the line."
  (interactive)
  (let ((note-path (linenote--get-candidate-note-path)))
    (if (not (file-exists-p note-path))
        (error "No notes to remove from here")
      (condition-case _
          (progn
            (pop-to-buffer (find-file-noselect note-path) 'reusable-frames)
            (let ((do-remove (yes-or-no-p (format "Remove %S?" note-path))))
              (delete-window)
              (when do-remove
                (delete-file note-path)
                (linenote--mark-note (file-name-base note-path) :remove))))
        (quit (delete-window))))))

(defun linenote--directory-files ()
  "Do `directory-files' to find notes (except for files with names ending with ~)."
  (directory-files (expand-file-name (or (file-name-directory (linenote--get-relpath)) "")
                                     (linenote--get-note-rootdir))
                   'full (concat (file-name-base (linenote--get-relpath)) ".[^.].*[^~]$")))

(defun linenote--remove-overlays-at (pos)
  "Remove overlays at POS by checking the `linenote--overlays'."
  (mapc (lambda (ov)
          (if (member ov linenote--overlays)
              (progn
                (delete-overlay ov)
                (delete ov linenote--overlays))))
        (overlays-at pos)))

(defun linenote--is-lock-file (file)
  "Check if FILE is a lock file."
  (string= (substring (file-name-base file) 0 2) ".#"))

(defun linenote--file-changed (event)
  "A function to handle file watch EVENT."
  (let* ((fs-id (nth 0 event))
         (etype (nth 1 event))
         (fpath (nth 2 event))
         (event-buffer (cdr (assoc fs-id linenote--buffers))))

    (when (and (string-match-p
                (regexp-quote (file-name-nondirectory
                               (buffer-file-name event-buffer)))
                (file-name-base fpath))
               (not (linenote--is-lock-file fpath)))
      (with-current-buffer event-buffer
        (cond
         ((string= etype "deleted")
          (linenote--mark-note fpath t))
         ((string= etype "created")
          (linenote--mark-note fpath)))))))

(defun linenote--dealloc-fswatch ()
  "Remove out the file watchers and corresponding list."
  (file-notify-rm-watch linenote--fwatch-id)
  (setq-local linenote--overlays nil)
  (setq linenote--buffers
        (delete (assoc linenote--fwatch-id linenote--buffers) linenote--buffers)))

(defun linenote--buffer-killed ()
  "A hook function for `kill-buffer-hook'."
  (linenote--dealloc-fswatch))

(defun linenote--remove-all-overlays ()
  "Remove all overlays in the current buffer."
  (mapc #'delete-overlay linenote--overlays))

(defun linenote--remove-all-fringes ()
  "Remove all fringes in the current buffer."
  (mapc #'delete-overlay linenote--fringe-markers))

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
                   (linenote--get-note-rootdir))))
    (make-directory note-dir t)

    ;; Set up some hooks.
    (add-hook 'kill-buffer-hook #'linenote--buffer-killed :local)
    (add-hook 'before-revert-hook #'linenote--remove-all-overlays :local)
    (add-hook 'before-revert-hook #'linenote--remove-all-fringes :local)

    ;; Set up a file watcher for the note directory.
    (let* ((buffer-id (current-buffer))
           ;; TODO `file-notify-add-watch' triggers an error if the file
           ;; cannot be watched. We should probably handle this.
           (watch-id (file-notify-add-watch note-dir
                                            '(change)
                                            #'linenote--file-changed)))
      (setq-local linenote--fwatch-id watch-id)
      (setq-local linenote--follow-cursor nil)
      (push `(,watch-id . ,buffer-id) linenote--buffers))

    ;; Mark all existing notes.
    (linenote-mark-notes)

    ;; Set up Eldoc.
    (when linenote-use-eldoc
      (setq-local eldoc-documentation-functions
                  (cons 'linenote--eldoc-show-buffer eldoc-documentation-functions)))))

(defun linenote--disable ()
  "Disable `linenote-mode' in the current buffer."
  (setq-local eldoc-documentation-functions
              (delete 'linenote--eldoc-show-buffer eldoc-documentation-functions))

  (remove-hook 'kill-buffer-hook #'linenote--buffer-killed :local)
  (remove-hook 'before-revert-hook #'linenote--remove-all-overlays :local)

  (linenote--remove-all-overlays)
  (linenote--remove-all-fringes)
  (linenote--dealloc-fswatch))

(defun linenote-find-root-dir ()
  "Open the linenote root directory for the current project."
  (interactive)
  (let ((note-dir (linenote--get-note-rootdir)))
    (if (file-exists-p note-dir)
        (find-file note-dir)
      (error "No notes found"))))

(defun linenote-find-note-dir ()
  "Open the note directory for the current file."
  (interactive)
  (let ((note-dir (expand-file-name (or (file-name-directory (linenote--get-relpath)) "")
                                    (linenote--get-note-rootdir))))
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
              (setq note (string-replace (expand-file-name ".linenote/"
                                                           (linenote--project-root))
                                         "" note)))
            (format "%-100s%s" note
                    (linenote--obtain-tag-string-by-key
                     (linenote--get-line-range-by-fname note))))
          notes))

(defun linenote--truncate-tags-or-spaces-from-string (str)
  "A function to truncate tags or spaces from STR."
  (car (string-split str " ")))

(defun linenote--eldoc-show-buffer (&optional args)
  "Show the first line of a candidate note in the mini-buffer.
Optional argument ARGS Return the string for eldoc.  Since we need
only note buffer, there is no usage of ARGS at all."
  (ignore args)
  (let ((note-path (linenote--get-candidate-note-path)))
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
                   (linenote--get-note-rootdir))))
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
                                  (linenote--get-note-rootdir))))

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
