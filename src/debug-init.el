;;; debug-init.el --- Debugging initialization file for Emacs

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
;; This initialization file starts up Emacs in a pristine state. It sets up the
;; `package-user-dir' variable with the path to the ELPA packages and exposes
;; `use-package' to make it easy for individual to be loaded manually.
;;
;; Usage:
;; emacs -Q -l /path/to/debug-init.el

;;; Code:

(setq package-user-dir (concat (file-name-directory load-file-name) "../elpa/src"))
(setq load-prefer-newer t)

(package-initialize)
(require 'use-package)

;;; debug-init.el ends here
