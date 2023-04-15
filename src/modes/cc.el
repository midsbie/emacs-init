;;; cc.el --- Configures C and C++ modes

;; Copyright (C) 2015-2023  Miguel Guedes

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

(defun init/cc/config ()
  "Initialise modes related to C and C++ development."

  (init/common-nonweb-programming-mode)
  (c-toggle-auto-hungry-state -1)

  (setq-local comment-start "/* ")
  (setq-local comment-end   " */"))

(use-package cc-mode
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c-ts-mode)
         ("\\.cpp\\'" . c++-ts-mode)
         ("\\.cxx\\'" . c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode)
         ("\\.hxx\\'" . c++-ts-mode))

  ;; buftoggle not found
  ;;  :bind (("C-c o" . buftoggle))

  :hook ((c-ts-mode . init/cc/config)
         (c++-ts-mode . init/cc-/config))

  :init
  ;; Set environment for compilers to use
  (when (executable-find "clang")
    (setenv "CC" "clang"))
  (when (executable-find "clang++")
    (setenv "CXX" "clang++")))

;;; cc.el ends here
