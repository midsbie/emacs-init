;;; prettier-js.el --- Configures the prettier-js minor mode

;; Copyright (C) 2017  Miguel Guedes

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


(add-hook 'js2-mode-hook 'init/prettier-js-mode-maybe)
(add-hook 'web-mode-hook 'init/prettier-js-mode-maybe)

;; Default parameters to pass to prettier.
(setq-default prettier-js-args
              '("--single-quote"
                "--trailing-comma" "es5"
                "--jsx-bracket-same-line"))

(defvar init/enable-prettier-js-mode)
(setq init/enable-prettier-js-mode nil)

(defun init/prettier-js-mode-maybe()
  (when init/enable-prettier-js-mode
    (prettier-js-mode)))

;;; pretiter-js.el ends here
