;;; init-flymake.el --- Configures the flymake feature  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Miguel Guedes

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
  "Enable Flymake for Emacs Lisp buffers, which do not use Eglot."
  (flymake-mode 1))

(use-package flymake
  :ensure nil
  :demand
  :diminish
  :hook (((emacs-lisp-mode lisp-interaction-mode) . init/flymake/enable))
  :bind ((:map flymake-mode-map
               ("C-c ! p" . flymake-goto-prev-error)
               ("C-c ! n" . flymake-goto-next-error)
               ("C-c ! l" . flymake-show-project-diagnostics)
               ("C-c ! b" . flymake-show-buffer-diagnostics)
               ("C-c ! c" . flymake-start))))

(provide 'init-flymake)

;;; init-flymake.el ends here
