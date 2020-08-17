;;; typescript.el --- Configures `typescript-mode'

;; Copyright (C) 2020  Miguel Guedes

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

(defun init/ts-tsx-mode ()
  "Initialise modes related to Typescript development."
  (init/common-web-programming)
  (electric-indent-mode)

  (setq-local fill-column 99)

  (local-set-key (kbd "M-a") 'c-beginning-of-statement)
  (local-set-key (kbd "M-e") 'c-end-of-statement))

(use-package typescript-mode
  :mode ("\\.tsx?\\'")
  :hook (typescript-mode . init/ts-tsx-mode))

;; ;;; typescript.el ends here
