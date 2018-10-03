;;; projectile.el --- Initialises the projectile package

;; Copyright (C) 2017-2018  Miguel Guedes

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

(defun init/projectile ()
  "Lazily initialise the `projectile' package."
    (require 'projectile)
    (projectile-mode)
)

; For some strange reason this now needs to be set otherwise the projectile
; keymap is not available at all.
(setq-default projectile-keymap-prefix "")

; This statement was producing an error when placed before the function it
; invokes, presumable because 'ido may have loaded in some circumstances.
(eval-after-load 'ido '(init/projectile))

;;; projectile.el ends here
