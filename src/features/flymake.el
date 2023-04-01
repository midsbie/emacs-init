;;; flymake.el --- Configures the flymake feature

;; Copyright (C) 2022-2023  Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun init/flymake/config ()
  "Configure flymake.

This function disables `flycheck-mode', if enabled, and
configures `flymake-eslint' correctly if it finds `eslint' in the
variable `exec-path'."
  (when (and (eglot-managed-p) flycheck-mode)
    (flycheck-mode -1))

  (flymake-diagnostic-at-point-mode 1)

  ;; This was taken from `flymake-eslint-enable' and may be necessary for the
  ;; checker to work.  Note that we shouldn't call this function here because
  ;; it'll lead to a never ending loop (it calls `flymake-mode').
  (unless flymake-eslint-defer-binary-check
    (flymake-eslint--ensure-binary-exists))
  (make-local-variable 'flymake-eslint-project-root)

  ;; Requires `init/add-node-modules-to-exec-path' to have been called during
  ;; mode configuration.  Enabling `flymake-eslint' by directly mutating
  ;; `flymake-diagnostic-functions' because calling `flymake-eslint-enable'
  ;; doesn't seem to work.
  (when (and (boundp 'flymake-eslint-executable-name)
             (executable-find flymake-eslint-executable-name))
    (ignore-errors
      (setq-local flymake-eslint-project-root (project-root (project-current))))
    (add-hook 'flymake-diagnostic-functions 'flymake-eslint--checker nil t)))

(use-package flymake-diagnostic-at-point
  :demand
  :after flymake
  :hook ((flymake-mode . init/flymake/config))
  :bind ((:map flymake-mode-map
               ("C-c ! p" . flymake-goto-prev-error)
               ("C-c ! n" . flymake-goto-next-error)
               ("C-c ! l" . flymake-show-project-diagnostics)
               ("C-c ! b" . flymake-show-buffer-diagnostics)
               ("C-c ! c" . flymake-start))))

;;; flymake.el ends here
