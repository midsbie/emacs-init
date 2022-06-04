;;; display-line-numbers.el --- Configures `display-line-numbers-mode-mode'

;; Copyright (C) 2015-2021  Miguel Guedes

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

;; [130921] Reinstating this mode and turning off `linum-mode' after running
;; `profile-start' and realising that the latter mode is super slow.  Then
;; found multiple references online confirming this and encouraging a switch to
;; this mode.
;;
;; References:
;; https://www.reddit.com/r/orgmode/comments/e7pq7k/linummode_very_slow_for_large_org_files/
;; https://www.emacswiki.org/emacs/LineNumbers
;;
;; [170920] linum-mode reinstated due to unresolved incompatibilities between
;; linu-numbers-mode and company-mode.  Turns out linum-mode also occupies less
;; space in the fringe, allowing more text and windows to fit in a single
;; frame.

;;; Code:

(defun init/display-line-numbers-mode ()
  "Lazily load the `display-line-numbers-mode' package and initialise it."

  ;; display line numbers in left margin
  (global-display-line-numbers-mode t))

(use-package display-line-numbers
  :config (init/display-line-numbers-mode))

;;; display-line-numbers.el ends here
