;;; prettier-js.el --- Configures the prettier-js minor mode

;; Copyright (C) 2017-2018  Miguel Guedes

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

(defun init/config/prettier-js()
  ;; Default parameters to pass to prettier.
  (setq-default prettier-js-args nil)

  (defvar init/enable-prettier-js-mode)
  (setq init/enable-prettier-js-mode nil))

(defun init/prettier-js-mode-maybe()
  (when init/enable-prettier-js-mode
    (prettier-js-mode)))

(use-package prettier-js
  :hook ((js-mode . init/prettier-js-mode-maybe)
         (js-jsx-mode . init/prettier-js-mode-maybe)
         (js2-mode . init/prettier-js-mode-maybe)
         (web-mode . init/prettier-js-mode-maybe))
  :config
  (init/config/prettier-js))

;;; prettier-js.el ends here
