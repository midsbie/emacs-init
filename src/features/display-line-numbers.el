;;; display-line-numbers-mode.el --- Configures `display-line-numbers-mode-mode'

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

;; [17092020] linum-mode reinstated due to unresolved incompatibilities between
;; linu-numbers-mode and company-mode.  Turns out linum-mode also occupies less
;; space in the fringe, allowing more text and windows to fit in a single
;; frame.

;;; Code:

(defun init/display-line-numbers-mode ()
  "Lazily load the `display-line-numbers-mode' package and initialise it."

  ;; display line numbers in left margin
  ; (global-display-line-numbers-mode t))

  ;; Currently disabled in favour of `linum-mode'.
  (global-display-line-numbers-mode -1))

(defun init/display-line-numbers-mode-mode()
  "Configure the `display-line-numbers-mode-mode'."
  (if (string= major-mode "speedbar-mode")
      (display-line-numbers-mode-mode -1)))

(use-package display-line-numbers
  :config
  (init/display-line-numbers-mode))

;;; display-line-numbers-mode.el ends here