;;; speedbar.el --- Configures the `speedbar' package

;; Copyright (C) 2015-2022  Miguel Guedes

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

(defun init/speedbar ()
  "Initialise `speedbar' package."
  (setq speedbar-frame-parameters '((minibuffer . nil)
                                    (width . 40)
                                    (border-width . 0)
                                    (menu-bar-lines . 0)
                                    (tool-bar-lines . 0)
                                    (unsplittable . t)
                                    (left-fringe . 0))))

(use-package speedbar
  :init (init/speedbar))

;;; speedbar.el ends here
