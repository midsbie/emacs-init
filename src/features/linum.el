;;; linum.el --- Configures `linum-mode'

;; Copyright (C) 2015-2024  Miguel Guedes

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

;; [11092020] linum-mode has been deprecated in favour of the faster
;; `display-line-numbers-mode' introduced in Emacs 26.
;;
;; [17092020] linum-mode reinstated due to unresolved incompatibilities between
;; linu-numbers-mode and company-mode.  Turns out linum-mode also occupies less
;; space in the fringe, allowing more text and windows to fit in a single
;; frame.

;;; Code:

(defun init/linum/config ()
  "Lazily load the `linum' package and initialise it."

  ;; display line numbers in left margin
  (global-linum-mode t))

(defun init/linum-mode/enable()
  "Configure the `linum-mode'."
  (if (string= major-mode "speedbar-mode")
      (linum-mode -1)))

;; Disabled in favour of `display-line-numbers':
(use-package linum
  :disabled
  :config (init/linum/config)
  :hook (linum-before-numbering-hook . init/linum-mode/enable))

;;; linum.el ends here
