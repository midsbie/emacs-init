;;; etc.el --- Assorted extra features

;; Copyright (C) 2020  Miguel Guedes

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

(defun clear-before-save-hooks()
  "Clear all `before-save-hook' lists in all buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (progn
      (with-current-buffer buf
        (setq before-save-hook nil)))))

(defun fix-indent-inverted-behaviour()
  "Fix RET and C-j inverted newline and indentation behaviour.

This function fixes the inverted indentation behaviour of RET and
C-j that occurs in major modes (e.g. web-mode).
"
  (local-set-key (kbd "C-j") 'newline-and-indent)
  (local-set-key (kbd "RET") 'newline))

;; The following meant to be exposed as a command; do not prefix the function name.
(defun google(query)
  "Open Google search with QUERY as parameter.

Prompts the user for a query string, if not provided, that is
constructed as a URL that causes the current browser provider to
run and navigate to the Google search page showing results for
the specified query string."
  (interactive "sQuery string: ")
  (browse-url (concat "https://google.com/search?q="
               (mapconcat 'identity (split-string query " ") "+"))))

;;; etc.el ends here