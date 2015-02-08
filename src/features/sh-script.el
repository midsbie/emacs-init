;;; sh-script.el --- Configures the sh-script feature

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

(require 'sh-script)

(setq-default  sh-basic-offset    2
               sh-indentation     2)

(defun init-sh-script ()
  "Initialise modes related to shell scripting development."
  (auto-fill-mode -1))

(add-hook 'sh-mode-hook 'init-common-programming)
(add-hook 'sh-mode-hook 'init-sh-script)

;;; sh-script.el ends here
