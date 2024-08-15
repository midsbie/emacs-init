;;; debug.el --- Utility functions for debugging     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun debug-message (format-string &rest args)
  "Print a debugging message to the *Messages* buffer.

Does not interfer with the minibuffer and keeps the special *Messages*
buffer scrolled to the bottom."
  (with-current-buffer (get-buffer-create "*Messages*")
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "%s\n" (apply 'format format-string args))))
    (let ((win (get-buffer-window (current-buffer))))
      (when win
        (set-window-point win (point-max))))))

(defun format-like-print (object)
  "Format an object into a string, similar to how `print` would output it."
  (cond
   ((stringp object)       ; Print strings with quotes
    (format "%S" object))
   ((listp object)         ; Recursively format list elements
    (format "'%s" (mapconcat #'format-like-print object " ")))
   ((vectorp object)       ; Convert vector to list and format
    (format "[ %s ]" (mapconcat #'format-like-print (append object nil) " ")))
   ((or (numberp object)   ; Numbers and symbols can be converted directly
        (symbolp object)
        t)                 ; Fallback for other types
    (format "%s" object))))

;;; debug.el ends here
