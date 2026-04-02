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
(defun my/modeline--compute-buffer-status ()
  "Compute (SIGN . FACE) cons for the buffer's file status."
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

(defvar-local my/modeline--buffer-status-cache nil
  "Cached (SIGN . FACE) cons from `my/modeline--compute-buffer-status'.")

(defvar-local my/modeline--buffer-status-modified-p 'unset
  "Value of `buffer-modified-p' when the status cache was last computed.")

(defun my/modeline--recompute-buffer-status (&rest _)
  "Recompute and cache the buffer file status."
  (setq my/modeline--buffer-status-modified-p (buffer-modified-p)
        my/modeline--buffer-status-cache (my/modeline--compute-buffer-status)))

(defun my/modeline--buffer-status ()
  "Return a cached (SIGN . FACE) cons for the buffer's file status.
Checks `buffer-modified-p' as a cheap invalidation guard so that
modified/unmodified transitions (including undo) reflect instantly."
  (if (and my/modeline--buffer-status-cache
           (eq (buffer-modified-p) my/modeline--buffer-status-modified-p))
      my/modeline--buffer-status-cache
    (my/modeline--recompute-buffer-status)))

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
(defun my/modeline--compute-flymake ()
  "Compute colorized flymake diagnostic counts: E/W/N."
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

(defvar-local my/modeline--flymake-cache 'unset
  "Cached flymake diagnostic string for mode-line display.
Value is `unset' when not yet computed.")

(defun my/modeline--recompute-flymake (&rest _)
  "Recompute and cache the flymake diagnostic string."
  (setq my/modeline--flymake-cache (my/modeline--compute-flymake)))

(defun my/modeline--on-flymake-mode ()
  "Update flymake cache when `flymake-mode' is toggled."
  (my/modeline--recompute-flymake))

(defun my/modeline-flymake ()
  "Return cached flymake diagnostic counts."
  (if (eq my/modeline--flymake-cache 'unset)
      (my/modeline--recompute-flymake)
    my/modeline--flymake-cache))

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

;; -- Global minor mode ---------------------------------------------------------

(defvar my/modeline--idle-timer nil
  "Idle timer for periodic buffer status recomputation.")

(defvar my/modeline--saved-mode-line-format nil
  "Saved `mode-line-format' default, restored when the mode is disabled.")

(defvar my/modeline--saved-flymake-ml-format 'unset
  "Saved `flymake-mode-line-format', restored when the mode is disabled.")

(defvar my/modeline--saved-flycheck-ml 'unset
  "Saved `flycheck-mode-line', restored when the mode is disabled.")

;; Based on the Emacs default, with two changes:
;; - mode-line-modified removed (buffer-identification handles status
;;   indicators)
;; - Right-aligned flymake/flycheck diagnostic counters appended
(defvar my/modeline--format
  `("%e"
    mode-line-front-space
    (:propertize ("" mode-line-mule-info mode-line-client mode-line-remote)
                 display (min-width (5.0)))
    mode-line-frame-identification
    (:eval (my/modeline-buffer-identification))
    "   "
    mode-line-position
    (:eval (my/modeline-vcs))
    " "
    (:propertize ("" mode-name)
                 face my/modeline-major-mode
                 mouse-face mode-line-highlight
                 local-map ,mode-line-major-mode-keymap)
    ("" mode-line-process)
    (:propertize ("" minor-mode-alist)
                 mouse-face mode-line-highlight
                 local-map ,mode-line-minor-mode-keymap)
    mode-line-misc-info
    mode-line-format-right-align
    (:eval (when-let* ((s (my/modeline-flymake))) (concat s " ")))
    (:eval (when-let* ((s (my/modeline-flycheck))) (concat s " ")))
    mode-line-end-spaces)
  "Custom mode-line format used by `my/modeline-mode'.")

(defun my/modeline--setup-flymake ()
  "Integrate with flymake: suppress its mode-line entry and wire cache updates."
  (when (eq my/modeline--saved-flymake-ml-format 'unset)
    (setq my/modeline--saved-flymake-ml-format flymake-mode-line-format)
    (setq flymake-mode-line-format nil)
    (add-hook 'flymake-mode-hook #'my/modeline--on-flymake-mode)
    ;; No public hook exists for "diagnostics updated"; advise the
    ;; internal entry point.  If a future Emacs adds one, switch to it.
    (advice-add 'flymake--handle-report :after #'my/modeline--recompute-flymake)
    (advice-add 'flymake-start :after #'my/modeline--recompute-flymake)))

(defun my/modeline--teardown-flymake ()
  "Remove flymake integration."
  (unless (eq my/modeline--saved-flymake-ml-format 'unset)
    (setq flymake-mode-line-format my/modeline--saved-flymake-ml-format
          my/modeline--saved-flymake-ml-format 'unset)
    (remove-hook 'flymake-mode-hook #'my/modeline--on-flymake-mode)
    (advice-remove 'flymake--handle-report #'my/modeline--recompute-flymake)
    (advice-remove 'flymake-start #'my/modeline--recompute-flymake)))

(defun my/modeline--setup-flycheck ()
  "Integrate with flycheck: suppress its mode-line entry."
  (when (eq my/modeline--saved-flycheck-ml 'unset)
    (setq my/modeline--saved-flycheck-ml flycheck-mode-line
          flycheck-mode-line nil)))

(defun my/modeline--teardown-flycheck ()
  "Remove flycheck integration."
  (unless (eq my/modeline--saved-flycheck-ml 'unset)
    (setq flycheck-mode-line my/modeline--saved-flycheck-ml
          my/modeline--saved-flycheck-ml 'unset)))

(define-minor-mode my/modeline-mode
  "Global minor mode for custom mode-line display.
Provides a project-aware colorized buffer name and compact diagnostic
counters.  All other segments use Emacs defaults."
  :global t
  :group 'init
  (if my/modeline-mode
      (progn
        (setq my/modeline--saved-mode-line-format (default-value 'mode-line-format))
        (setq-default mode-line-format my/modeline--format)
        (advice-add 'vc-refresh-state :after #'my/modeline--update-project-root)
        (setq my/modeline--idle-timer
              (run-with-idle-timer 1 t #'my/modeline--recompute-buffer-status))
        (when (featurep 'flymake) (my/modeline--setup-flymake))
        (when (featurep 'flycheck) (my/modeline--setup-flycheck))
        (force-mode-line-update t))
    (when (timerp my/modeline--idle-timer)
      (cancel-timer my/modeline--idle-timer)
      (setq my/modeline--idle-timer nil))
    (advice-remove 'vc-refresh-state #'my/modeline--update-project-root)
    (when (featurep 'flymake) (my/modeline--teardown-flymake))
    (when (featurep 'flycheck) (my/modeline--teardown-flycheck))
    (setq-default mode-line-format my/modeline--saved-mode-line-format)
    (force-mode-line-update t)))

;; Deferred integration: set up when libraries load (if mode is active).
(with-eval-after-load 'flymake
  (when my/modeline-mode (my/modeline--setup-flymake)))
(with-eval-after-load 'flycheck
  (when my/modeline-mode (my/modeline--setup-flycheck)))

(my/modeline-mode 1)

;;; modeline.el ends here
