;;; flymake.el --- Configures the flymake feature

;; Copyright (C) 2022  Miguel Guedes

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
  (when flycheck-mode
    (flycheck-mode -1))

  (flymake-diagnostic-at-point-mode 1)

  ;; Requires `add-node-modules-path' to have been called during mode
  ;; configuration.  Calling `add-hook' to enable `flymake-eslint' because
  ;; `flymake-eslint-enable' doesn't seem to work.
  (when (executable-find flymake-eslint-executable-name)
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
