;;; sh.el --- Configures sh-mode

;; Copyright (C) 2015-2022  Miguel Guedes

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

(defun init/sh-script ()
  "Configure the `sh-script' package."
  (setq-default  sh-basic-offset    2
                 sh-indentation     2))

(defun init/config/sh-mode ()
  "Initialise modes related to shell scripting development."
  (init/common-nonweb-programming-mode)
  (auto-fill-mode -1))

(use-package sh-script
  :init (init/sh-script)
  :hook ((sh-mode . init/config/sh-mode)))

;;; sh.el ends here
