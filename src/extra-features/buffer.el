;;; buffer.el --- Extra features for buffer manipulation

;; Copyright (C) 2023  Miguel Guedes

;; Author: Miguel Guedes <miguel@softgeist.com>
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

(defun my/beginning-of-statement ()
  "Safe version of `c-beginning-of-statement-1'.

Attempts to run the original function.  In web-related modes like
`typescript-ts-mode' errors may occur, in which case it will
attempt to provide a similar result using alternative methods."
  (interactive)
  (execute-first-in-list init/beginning-of-statement-fn))

(defun my/end-of-statement ()
  "Safe version of `c-end-of-statement'.

Attempts to run the original function.  In web-related modes like
`typescript-ts-mode' errors may occur, in which case it will
attempt to provide a similar result using alternative methods."
  (interactive)
  (execute-first-in-list init/end-of-statement-fn))

;;; buffer.el ends here
