;;; term.el --- Initialisation sequence when run from the terminal

;; Copyright (C) 2014-2026 Miguel Guedes

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

;;; Commentary:

;; Loaded by `init.el' when `display-graphic-p' is nil (terminal Emacs).
;; All forms are evaluated at top level on load.

;;; Code:

;; Load wombat as the base, then layer our terminal theme on top.
(add-to-list 'custom-theme-load-path
             (concat init/path-base "environment"))
(load-theme 'wombat t)
(load-theme 'my-term-dark t)

;; Enable system clipboard integration in terminal (Emacs 29+)
(if (package-installed-p 'xclip)
    (use-package xclip
      :config
      (xclip-mode 1))
  (warn "Package 'xclip' is not available; clipboard integration disabled"))


;;; term.el ends here
