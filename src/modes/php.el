;;; php.el --- Configures `php-mode'

;; Copyright (C) 2015-2024  Miguel Guedes

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

(defun init/php-mode ()
  "Initialise `php-mode'."
  (setq-default php-mode-coding-style      'default ; DISABLED: drupal
                php-lineup-cascaded-calls  t
                flycheck-phpmd-rulesets
                '("cleancode" "codesize" "unusedcode" "design"))

  (c-add-style "default-php" '("drupal")))

(defun init/php-mode/enable ()
  "Configure `php-mode'."
  (init/common-programming-mode)

  (setq-local comment-start "/* ")
  (setq-local comment-end   " */")

  (eldoc-mode 1)
  (c-set-style "default-php")
  (c-toggle-auto-newline -1))

(use-package php-ts-mode
  :mode "\\.php\\'"
  :hook ((php-mode php-ts-mode) . init/php-mode/enable)
  :init (init/php-mode))

;;; php.el ends here
