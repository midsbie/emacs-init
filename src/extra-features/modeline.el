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

;; Lightweight mode-line customization providing a project-aware colorized
;; buffer name and compact diagnostic counters.  All other segments use Emacs
;; defaults.
;;
;; All faces inherit from standard Emacs faces so they adapt to any theme.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; -- Faces: major mode --------------------------------------------------------
(defface my/modeline-major-mode
  '((t (:inherit font-lock-function-name-face)))
  "Face for major mode name in mode line."
  :group 'init)

;; -- Faces: buffer identification ---------------------------------------------
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

;; -- Faces: diagnostics -------------------------------------------------------
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

;; -- Faces: VC state ----------------------------------------------------------
(custom-theme-set-faces
 'user
 '(vc-up-to-date-state    ((t (:inherit shadow))))
 '(vc-edited-state        ((t (:inherit success))))
 '(vc-locally-added-state ((t (:inherit font-lock-constant-face))))
 '(vc-needs-update-state  ((t (:inherit warning))))
 '(vc-conflict-state      ((t (:inherit (bold error)))))
 '(vc-missing-state       ((t (:inherit error))))
 '(vc-removed-state       ((t (:inherit warning))))
 '(vc-locked-state        ((t (:inherit font-lock-constant-face))))
 '(vc-ignored-state       ((t (:inherit shadow)))))

;; -- Major mode face ----------------------------------------------------------
(defun my/modeline--fontify-mode-name ()
  "Propertize `mode-name' with `my/modeline-major-mode' face."
  (setq mode-name (propertize (format-mode-line mode-name)
                               'face 'my/modeline-major-mode)))

(add-hook 'after-change-major-mode-hook #'my/modeline--fontify-mode-name)

;; -- Segment: VCS branch ------------------------------------------------------
(defun my/modeline-vcs ()
  "Return VC branch name without the backend prefix.
The vc-state face applied by VC is preserved, so the branch name
color reflects the file's version-control state."
  (when (and vc-mode (buffer-file-name))
    (let ((str (string-trim (format-mode-line vc-mode))))
      (when (> (length str) 0)
        ;; vc-mode format: "Git-branch", "Git:branch", "Git!branch", etc.
        ;; Strip the backend name and state separator.
        (concat " " (if (string-match "\\`\\w+[-:!?@]" str)
                        (substring str (match-end 0))
                      str))))))

;; -- Segment: buffer identification -------------------------------------------
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

(defvar-local my/modeline--project-root 'unset
  "Cached project root for mode line display.
Value is `unset' when not yet computed, nil when no project was found,
or a string with the project root path.")

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
         (project (if (eq my/modeline--project-root 'unset)
                      (my/modeline--update-project-root)
                    my/modeline--project-root))
         (status (my/modeline--buffer-status))
         (sign (car status))
         (status-face (cdr status))
         (name-end (file-name-nondirectory (buffer-name)))
         ;; Build the path prefix
         (path-prefix
          (cond
           ;; No file: just use buffer name
           ((not filepath) "")
           ;; Show project-name/abbreviated-path/ in a project.  Project name is
           ;; omitted when eglot is active (it shows it already.)
           ((and project (string-prefix-p project filepath))
            (let* ((eglot-p (bound-and-true-p eglot--managed-mode))
                   (dir (file-name-nondirectory (directory-file-name project)))
                   (rel (string-remove-prefix project filepath))
                   (parts (butlast (split-string rel "/" t))))
              (concat
               (unless eglot-p
                 (concat (propertize dir 'face 'my/modeline-project) "/"))
               (when parts
                 (concat
                  (propertize
                   (mapconcat (lambda (s) (substring s 0 1)) parts "/")
                   'face 'my/modeline-path)
                  "/")))))
           ;; File is outside project so show parent dir
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

;; -- Segment: flymake diagnostics ---------------------------------------------
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

;; -- Segment: flycheck diagnostics --------------------------------------------
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

;; -- Suppress built-in diagnostic mode-line entries ---------------------------
(with-eval-after-load 'flymake
  (setq flymake-mode-line-format nil))

(with-eval-after-load 'flycheck
  (setq flycheck-mode-line nil))

;; -- Assemble mode-line-format ------------------------------------------------
;; Based on the Emacs default, with two changes:
;; - mode-line-modified removed (buffer-identification handles status
;;   indicators)
;; - Right-aligned flymake/flycheck diagnostic counters appended
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                (:propertize ("" mode-line-mule-info mode-line-client mode-line-remote)
                             display (min-width (5.0)))
                mode-line-frame-identification
                (:eval (my/modeline-buffer-identification))
                "   "
                mode-line-position
                (:eval (my/modeline-vcs))
                "  "
                mode-line-modes
                mode-line-misc-info
                mode-line-format-right-align
                (:eval (when-let* ((s (my/modeline-flymake))) (concat s " ")))
                (:eval (when-let* ((s (my/modeline-flycheck))) (concat s " ")))
                mode-line-end-spaces))

;;; modeline.el ends here
