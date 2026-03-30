;;; modeline.el --- Custom mode-line with colorized segments  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: faces, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Custom mode-line inspired by the mini-echo package, providing colorized
;; segments with intelligent buffer name display, VC integration, diagnostic
;; counters, and contextual indicators (macro recording, narrowing, selection,
;; remote host, process status).
;;
;; All faces inherit from standard Emacs faces (error, warning, success, shadow,
;; font-lock-*) so they adapt automatically to any theme.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; ---------------------------------------------------------------------------
;; Faces — inherit from standard Emacs faces for theme portability
;; ---------------------------------------------------------------------------
(defface my/modeline-major-mode
  '((t (:inherit font-lock-function-name-face)))
  "Face for major mode name in mode line."
  :group 'init)

(defface my/modeline-buffer-name
  '((t (:weight bold)))
  "Face for buffer name in mode line."
  :group 'init)

(defface my/modeline-project
  '((t (:inherit font-lock-keyword-face)))
  "Face for project name in mode line."
  :group 'init)

(defface my/modeline-path
  '((t (:inherit shadow)))
  "Face for abbreviated intermediate directories in mode line."
  :group 'init)

(defface my/modeline-modified
  '((t (:inherit (bold success))))
  "Face for buffer modified indicator."
  :group 'init)

(defface my/modeline-modified-external
  '((t (:inherit (bold warning))))
  "Face for buffer modified externally indicator."
  :group 'init)

(defface my/modeline-missing
  '((t (:inherit (bold error))))
  "Face for missing file indicator."
  :group 'init)

(defface my/modeline-read-only
  '((t (:inherit (bold warning))))
  "Face for read-only indicator."
  :group 'init)

(defface my/modeline-position
  '((t (:inherit font-lock-constant-face)))
  "Face for cursor position in mode line."
  :group 'init)

(defface my/modeline-diagnostic-error
  '((t (:inherit (bold error))))
  "Face for error count in mode line."
  :group 'init)

(defface my/modeline-diagnostic-warning
  '((t (:inherit (bold warning))))
  "Face for warning count in mode line."
  :group 'init)

(defface my/modeline-diagnostic-note
  '((t (:inherit (bold font-lock-type-face))))
  "Face for note/info count in mode line."
  :group 'init)

(defface my/modeline-diagnostic-running
  '((t (:inherit shadow)))
  "Face for running/checking indicator."
  :group 'init)

(defface my/modeline-status-local
  '((t (:inherit (bold warning))))
  "Face for local buffer status indicators (selection, narrow, etc.)."
  :group 'init)

(defface my/modeline-status-global
  '((t (:inherit (bold font-lock-type-face))))
  "Face for global status indicators (macro, process, etc.)."
  :group 'init)

(defface my/modeline-remote
  '((t (:inherit (bold error))))
  "Face for remote host indicator."
  :group 'init)

(defface my/modeline-eglot-active
  '((t (:inherit success :weight normal)))
  "Face for active eglot indicator."
  :group 'init)

;; ---------------------------------------------------------------------------
;; Segment: Buffer name with project path, VC, and status
;; ---------------------------------------------------------------------------
(defun my/modeline--buffer-status ()
  "Return a (SIGN . FACE) cons for the buffer's file status."
  (let* ((file-name (buffer-file-name))
         (file-remote (and file-name (file-remote-p file-name)))
         (file-exist (and file-name (not file-remote) (file-exists-p file-name))))
    (cond
     ((and file-exist (not (verify-visited-file-modtime)))
      (cons "!" 'my/modeline-modified-external))
     ((and (or file-exist file-remote) (buffer-modified-p))
      (cons "*" 'my/modeline-modified))
     ((and file-name (not file-exist) (not file-remote))
      (cons "?" 'my/modeline-missing))
     (t (cons "" nil)))))

(defvar-local my/modeline--project-root nil
  "Cached project root for mode line display.")

(defun my/modeline--update-project-root ()
  "Update the cached project root."
  (setq my/modeline--project-root
        (when-let* (((fboundp 'project-current))
                    (project (project-current)))
          (expand-file-name (project-root project)))))

(advice-add 'vc-refresh-state :after #'my/modeline--update-project-root)

(defun my/modeline-buffer-identification ()
  "Return a colorized buffer name with project path and status.
Shows: [read-only] [project/a/b/c/]filename[status]"
  (let* ((ro (when buffer-read-only
               (propertize "%% " 'face 'my/modeline-read-only)))
         (filepath (buffer-file-name))
         (project (or my/modeline--project-root
                      (my/modeline--update-project-root)))
         (status (my/modeline--buffer-status))
         (sign (car status))
         (status-face (cdr status))
         (name-end (file-name-nondirectory (buffer-name)))
         ;; Build the path prefix
         (path-prefix
          (cond
           ;; No file — just use buffer name
           ((not filepath) "")
           ;; In a project — show project-name/abbreviated-path/
           ((and project (string-prefix-p project filepath))
            (let* ((dir (file-name-nondirectory (directory-file-name project)))
                   (rel (string-remove-prefix project filepath))
                   (parts (butlast (split-string rel "/" t))))
              (concat
               (propertize dir 'face 'my/modeline-project)
               "/"
               (when parts
                 (concat
                  (propertize
                   (mapconcat (lambda (s) (substring s 0 1)) parts "/")
                   'face 'my/modeline-path)
                  "/")))))
           ;; File outside project — show parent dir
           (filepath
            (propertize (concat (file-name-nondirectory
                                (directory-file-name default-directory))
                               "/")
                        'face 'my/modeline-path))
           (t "")))
         ;; Colorized filename + sign
         (name-str (if status-face
                       (propertize (concat name-end sign) 'face status-face)
                     (propertize name-end 'face 'my/modeline-buffer-name))))
    (concat (or ro "") path-prefix name-str)))

;; ---------------------------------------------------------------------------
;; Segment: VCS branch
;; ---------------------------------------------------------------------------
(defvar my/modeline-vcs-max-length 15
  "Maximum length for the VCS branch display.")

(defun my/modeline-vcs ()
  "Return VCS branch info, preserving vc-state face colors."
  (when (and vc-mode (buffer-file-name))
    (let* ((str (string-trim (format-mode-line vc-mode)))
           (len (length str))
           (str (if (and my/modeline-vcs-max-length (> len my/modeline-vcs-max-length))
                    (concat (substring str 0 (- my/modeline-vcs-max-length 2)) "..")
                  str)))
      (when (> (length str) 0)
        (concat " " str)))))

;; ---------------------------------------------------------------------------
;; Segment: Major mode
;; ---------------------------------------------------------------------------
(defun my/modeline-major-mode ()
  "Return colorized major mode name."
  (propertize
   (substring-no-properties
    (pcase major-mode
      ('dired-mode "Dired")
      (_ (format-mode-line mode-name))))
   'face 'my/modeline-major-mode))

;; ---------------------------------------------------------------------------
;; Segment: Eglot status
;; ---------------------------------------------------------------------------
(defun my/modeline-eglot ()
  "Return eglot server status."
  (when (bound-and-true-p eglot--managed-mode)
    (propertize " eglot" 'face 'my/modeline-eglot-active)))

;; ---------------------------------------------------------------------------
;; Segment: Flymake diagnostics
;; ---------------------------------------------------------------------------
(defun my/modeline-flymake ()
  "Return colorized flymake diagnostic counts: E/W/N."
  (when (bound-and-true-p flymake-mode)
    (let ((errn 0) (warnn 0) (noten 0)
          (running (flymake-running-backends)))
      (dolist (d (flymake-diagnostics))
        (pcase (get (flymake-diagnostic-type d) 'flymake-category)
          ('flymake-error (cl-incf errn))
          ('flymake-warning (cl-incf warnn))
          (_ (cl-incf noten))))
      (concat
       (when running (propertize "* " 'face 'my/modeline-diagnostic-running))
       (propertize (number-to-string errn) 'face 'my/modeline-diagnostic-error)
       "/"
       (propertize (number-to-string warnn) 'face 'my/modeline-diagnostic-warning)
       "/"
       (propertize (number-to-string noten) 'face 'my/modeline-diagnostic-note)))))

;; ---------------------------------------------------------------------------
;; Segment: Flycheck diagnostics
;; ---------------------------------------------------------------------------
(declare-function flycheck-count-errors "ext:flycheck")

(defun my/modeline-flycheck ()
  "Return colorized flycheck diagnostic counts: E/W/I."
  (when (bound-and-true-p flycheck-mode)
    (let* ((running (eq flycheck-last-status-change 'running))
           (errors (when flycheck-current-errors
                     (flycheck-count-errors flycheck-current-errors)))
           (errn (or (alist-get 'error errors) 0))
           (warnn (or (alist-get 'warning errors) 0))
           (infon (or (alist-get 'info errors) 0)))
      (concat
       (when running
         (propertize "* " 'face 'my/modeline-diagnostic-running))
       (propertize (number-to-string errn) 'face 'my/modeline-diagnostic-error)
       "/"
       (propertize (number-to-string warnn) 'face 'my/modeline-diagnostic-warning)
       "/"
       (propertize (number-to-string infon) 'face 'my/modeline-diagnostic-note)))))

;; ---------------------------------------------------------------------------
;; Segment: Position (line:col)
;; ---------------------------------------------------------------------------
(defun my/modeline-position ()
  "Return colorized cursor position L:C."
  (propertize (format-mode-line "%l:%c") 'face 'my/modeline-position))

;; ---------------------------------------------------------------------------
;; Segment: Buffer size
;; ---------------------------------------------------------------------------
(defun my/modeline-buffer-size ()
  "Return buffer size string."
  (format-mode-line "%I"))

;; ---------------------------------------------------------------------------
;; Segment: Remote host
;; ---------------------------------------------------------------------------
(defun my/modeline-remote-host ()
  "Return @hostname when visiting a remote file."
  (when-let* ((file default-directory)
              (host (file-remote-p file 'host)))
    (propertize (concat " @" host) 'face 'my/modeline-remote)))

;; ---------------------------------------------------------------------------
;; Segment: Macro recording
;; ---------------------------------------------------------------------------
(defun my/modeline-macro ()
  "Return [Def] indicator when recording a keyboard macro."
  (when defining-kbd-macro
    (propertize " [Def]" 'face 'my/modeline-status-global)))

;; ---------------------------------------------------------------------------
;; Segment: Narrowing
;; ---------------------------------------------------------------------------
(defun my/modeline-narrow ()
  "Return [Narrow] indicator when buffer is narrowed."
  (when (buffer-narrowed-p)
    (propertize " [Narrow]" 'face 'my/modeline-status-local)))

;; ---------------------------------------------------------------------------
;; Segment: Selection info
;; ---------------------------------------------------------------------------
(defun my/modeline-selection ()
  "Return selection info when region is active."
  (when (use-region-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (chars (abs (- end beg)))
           (lines (count-lines beg end)))
      (propertize
       (if (> lines 1)
           (format " %dC,%dL" chars lines)
         (format " %dC" chars))
       'face 'my/modeline-status-local))))

;; ---------------------------------------------------------------------------
;; Segment: Process
;; ---------------------------------------------------------------------------
(defun my/modeline-process ()
  "Return mode-line-process if active."
  (when (and mode-line-process
             (not (derived-mode-p 'sh-base-mode)))
    (let ((str (string-trim (format-mode-line mode-line-process))))
      (when (> (length str) 0)
        (propertize (concat " " str) 'face 'my/modeline-status-global)))))

;; ---------------------------------------------------------------------------
;; Assemble mode-line-format
;; ---------------------------------------------------------------------------
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                ;; Left side
                (:eval (my/modeline-buffer-identification))
                (:eval (my/modeline-vcs))
                " "
                (:eval (my/modeline-major-mode))
                (:eval (my/modeline-eglot))
                (:eval (my/modeline-remote-host))
                ;; Contextual indicators
                (:eval (my/modeline-macro))
                (:eval (my/modeline-narrow))
                (:eval (my/modeline-selection))
                (:eval (my/modeline-process))
                ;; Right-align remaining segments (Emacs 30+)
                mode-line-format-right-align
                ;; Right side: diagnostics
                (:eval (when-let* ((s (my/modeline-flymake))) (concat s " ")))
                (:eval (when-let* ((s (my/modeline-flycheck))) (concat s " ")))
                (:eval (my/modeline-position))
                " "
                (:eval (my/modeline-buffer-size))
                " "
                mode-line-end-spaces))

;;; modeline.el ends here
