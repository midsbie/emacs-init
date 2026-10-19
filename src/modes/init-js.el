;;; init-js.el --- Configures Javascript buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026  Miguel Guedes

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

;; Use js-ts-mode for JavaScript and JSX buffers.

;;; Code:

(defun init/js/enable ()
  "Configures modes related to Javascript development."
  (init/common-web-programming-mode))

(use-package js
  :ensure nil
  :mode (("\\.jsx?\\'" . js-ts-mode)
         ("\\.mjs\\'" . js-ts-mode))
  :interpreter (("node\\(?:js\\)?" . js-ts-mode))
  :custom
  (js-indent-level 2)
  :hook (((js-ts-mode js-mode js-jsx-mode) . init/js/enable)))

(provide 'init-js)

;;; init-js.el ends here
