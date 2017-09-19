;;; ansi-color.el --- Customises ANSI color palette

;; Copyright (C) 2015  Miguel Guedes

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

(eval-after-load 'eww '(init/eww))

;; Global key definitions
(global-set-key (kbd "C-c e g") 'google)

(defun init/eww ()
  "Lazily initialise the `eww' package.")

;; Extend the eww
(defun google (search)
  "Navigate to Google search within `eww'

Prompts the user for a query string, if not provided, that is constructed as a
URL that causes `eww' to run and navigate to the Google search page showing
results for the specified query string."
  (interactive "sQuery string: ")
  (eww (concat "https://google.com/search?q="
               (mapconcat 'identity (split-string search " ") "+"))))

;;; ansi-color.el ends here
