;;; php.el --- Configures `php-mode'

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

(require 'php-mode)

(setq-default php-mode-coding-style      'default ; DISABLED: drupal
              php-lineup-cascaded-calls  t)

(c-add-style "default-php"
             '("drupal"))

(defun init-php-mode ()
  "Initialise modes related to PHP development."
  (setq comment-start "/* "
        comment-end   " */")
  (eldoc-mode 1)
  (php-eldoc-enable)
  (c-set-style "default-php"))

(add-hook 'php-mode-hook 'init-php-mode)

;;; php.el ends here
