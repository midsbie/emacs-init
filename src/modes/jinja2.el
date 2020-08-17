;;; jinja2.el --- Configures `jinja2-mode'

;; Copyright (C) 2015-2020  Miguel Guedes

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

(defun init/config/jinja2 ()
  "Configure the `jinja2' package."
  (add-hook 'jinja2-mode-hook  'init/jinja2))

(defun init/jinja2-mode ()
  "Initialise the jinja2 mode."
  )

(use-package jinja2-mode
  :mode ("\\.j2?\\'")
  :config
  (init/config/jinja2))

;;; jinja2.el ends here
