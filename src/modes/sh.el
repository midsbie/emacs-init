;;; sh.el --- Configures sh-mode

;; Copyright (C) 2015-2026  Miguel Guedes

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

(defun init/sh ()
  "Initialize sh-related modes."
  (setq-default  sh-basic-offset    2
                 sh-indentation     2)

  ;; Prefer the tree-sitter mode.  `bash-ts-mode' hands scripts written for
  ;; other shells (zsh, csh, ...) back to `sh-mode' (`sh--redirect-bash-ts-mode').
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode)))

(defun init/sh/enable ()
  "Initialise modes related to shell scripting development."
  (init/common-nonweb-programming-mode))

(use-package sh-script
  :hook ((sh-mode bash-ts-mode) . init/sh/enable)
  :init (init/sh))

;;; sh.el ends here