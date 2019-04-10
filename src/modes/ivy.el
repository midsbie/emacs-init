;;; ivy.el --- Configures `ivy-mode'

;; Copyright (C) 2019  Miguel Guedes

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

(require 'ivy)
(require 'flx)

;; Activate ivy's global mode
(ivy-mode 1)

; From: https://www.reddit.com/r/emacs/comments/51lqn9/helm_or_ivy/d7d34il/
; Let ivy use flx for fuzzy-matching
(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

; Use Enter on a directory to navigate into the directory, not open it with dired.
(define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)

; Let projectile use ivy
(setq projectile-completion-system 'ivy)

;;; ivi.el ends here
