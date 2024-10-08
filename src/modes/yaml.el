;;; yaml.el --- Configures `yaml-mode'

;; Copyright (C) 2019-2024  Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: internal, tools

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

(defun init/yaml-mode/enable ()
  (init/common-programming-mode))

(use-package yaml-ts-mode
  :hook ((yaml-mode yaml-ts-mode) . init/yaml-mode/enable)
  :mode ("\\.yml\\'" "\\.yaml\\'" "\\.eslintrc\\'"))

;;; yaml.el ends here
