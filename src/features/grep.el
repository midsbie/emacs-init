;;; grep.el --- grep command extensions

;; Copyright (C) 2019  Miguel Guedes

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

(defun git-grep (query)
  "Run a grep search for QUERY using git.

When executed without the universal \
argument (\\[universal-argument]), runs \ \"git grep\" from the \
repository's root directory.  When the universal argument is \
specified, \"git grep\" is executed from the current working \
directory."
  (interactive "sgit grep: ")
  (print query)

  (let ((end))
    ; This gem about the presence of the universal argument was derived from
    ; the answer at https://stackoverflow.com/a/56853097
    (unless current-prefix-arg
      (setq end " -- :/"))
    (grep (concat "git --no-pager grep -n -e \"" query "\"" end)))
  )

;;; grep.el ends here
