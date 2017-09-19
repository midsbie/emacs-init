;;; linum.el --- Configures `linum-mode'

;; Copyright (C) 2015-2017  Miguel Guedes

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

(init/lazy-run 'init/linum)

(defun init/linum ()
  "Lazily load the `linum' package and initialise it."
  (load "linum")

  ;; display line numbers in left margin
  (global-linum-mode t)

  ;; Let's make sure we disable linum-mode when in speedbar-mode
  (defun init-linum-mode()
    (if (string= major-mode "speedbar-mode")
        (linum-mode -1)))

  (add-hook 'linum-before-numbering-hook 'init-linum-mode))

;;; linum.el ends here
