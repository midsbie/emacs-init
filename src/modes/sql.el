;;; sql.el --- Customises the sql Emacs package

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

(defun init/sql-interactive-mode/enable ()
  (auto-fill-mode -1)
  (display-fill-column-indicator-mode -1))

(defun init/sql-mode/enable ()
  (init/common-programming-mode))

(use-package sql
  :hook ((sql-interactive-mode . init/sql-interactive-mode/enable)
         (sql-mode . init/sql-mode/enable)))

;;; sql.el ends here
