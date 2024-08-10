;;; ranger.el --- Customises Ace Window package

;; Copyright (C) 2021-2024  Miguel Guedes

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

;; Useful Links:
;;
;; * Project hosted on Github:
;;   https://github.com/ralesi/ranger.el
;;
;; * Documentation:
;;   https://github.com/ralesi/ranger.el#basic-commands

;;; Code:

(defun init/ranger/config ()
  (setq ranger-show-literal nil))

(use-package ranger
  :config (init/ranger/config)
  :bind ("C-x D" . ranger))

;;; ranger.el ends here
