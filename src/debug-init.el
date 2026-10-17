;;; debug-init.el --- Debugging initialization file for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; URL:
;; Keywords: tools
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;; This is a specialized Emacs initialization file crafted to assist in
;; debugging Emacs configurations by providing a minimal and controlled
;; environment. It is particularly useful for isolating and identifying issues
;; within your Emacs setup by excluding external influences and focusing solely
;; on the configuration under scrutiny.
;;
;; This initialization file starts up Emacs in a pristine state. It applies the
;; package settings in early-init.el and activates the installed packages, so
;; that individual packages can be loaded manually with `use-package'.
;;
;; Usage:
;; emacs -Q -l /path/to/debug-init.el

;;; Code:

;; -Q skips both early-init.el and package activation.
(load (expand-file-name "early-init" (file-name-directory (file-truename load-file-name)))
      nil 'nomessage)
(package-activate-all)
(setq use-package-always-ensure t)

;;; debug-init.el ends here
