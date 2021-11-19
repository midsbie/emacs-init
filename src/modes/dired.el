;;; dired.el --- Configures `dired-mode'

;; Copyright (C) 2015-2021  Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: internal, tools

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

(defun init/dired-load-hook ()
  (use-package "dired-x")
  ;; Set global variables here.  For example:
  ;; (setq dired-guess-shell-gnutar "gtar")
  )

(defun init/dired-mode-hook ()
  ;; Set buffer-local variables here.  For example:
  ;; (dired-omit-mode 1)
  )

(use-package dired
  :hook ((dired-mode .init/dired-mode-hook)
         (dired-load-hook . init/dired-load-hook)))

;;; dired.el ends here
