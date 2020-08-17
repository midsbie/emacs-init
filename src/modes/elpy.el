;;; elpy.el --- Configures `elpy-mode'

;; Copyright (C) 2017-2020  Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: internal, tools

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
;; Note that for this Emacs package to be used, the following PIP packages must
;; also be installed, as given below:
;;
;; # sudo pip install jedi flake8 autopep8
;;

;;; Code:

;; Installation instructions straight from:
;; https://elpy.readthedocs.io/en/latest/introduction.html#installation
(use-package elpy
;  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

;;; elpy.el ends here
