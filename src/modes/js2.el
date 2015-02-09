;;; js2.el --- Configures `js2-mode'

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

(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default js2-basic-offset  2)

(add-hook 'js2-mode-hook  'init-common-programming)
(add-hook 'js2-mode-hook  'init-js2-mode)

(defun init-js2-mode ()
  "Customise js2-mode.
In particular, add responsive logic to indent the current line
whenever the dot character (.) is typed on a continued
expression."
  (setq comment-start "/*"
        comment-end   "*/")

  (local-set-key "."  '(lambda ()
                   (interactive)
                   (insert-char ?.)
                   (when (js2-continued-expression-p)
                     (indent-for-tab-command))))

  (local-set-key [f3]
                 '(lambda ()
                   (interactive)
                   (unless (and next-error-function
                                (not (string= (type-of (js2-next-error))
                                              "string")))
                     (flycheck-next-error)))))

;;; js2.el ends here
