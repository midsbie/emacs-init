;;; consts.el --- defconst and defaliases statements

;; Copyright (C) 2021  Miguel Guedes

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

;; I created this module as I wasn't able to get the `delight' package to hide
;; minor modes of my choosing.
;;
;; 211118 Now relying on use-package and the :diminish property.  Code kept for
;;        future reference.

;;; Code:

(setq my/diminished-modes '())

(dolist (mode my/diminished-modes)
  (add-hook (add-suffix-to-symbol mode "-hook") `(lambda ()
                                                   (diminish (quote ,mode)))))

;;; modeline.el ends here
