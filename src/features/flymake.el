;;; flymake.el --- Configures the flymake feature

;; Copyright (C) 2022-2024  Miguel Guedes

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

(defun init/flymake/enable ()
  "Configure flymake.

This function disables `flycheck-mode', if enabled, and
configures `flymake-eslint' correctly if it finds `eslint' in the
variable `exec-path'."
  (when (and (eglot-managed-p) (boundp 'flycheck-mode) flycheck-mode)
    (flycheck-mode -1))

  (flymake-diagnostic-at-point-mode 1))

(use-package flymake
  :demand
  :hook ((flymake-mode . init/flymake/enable)))

(use-package flymake-diagnostic-at-point
  :demand
  :after flymake
  :bind ((:map flymake-mode-map
               ("C-c ! p" . flymake-goto-prev-error)
               ("C-c ! n" . flymake-goto-next-error)
               ("C-c ! l" . flymake-show-project-diagnostics)
               ("C-c ! b" . flymake-show-buffer-diagnostics)
               ("C-c ! c" . flymake-start))))

;;; flymake.el ends here
