;;; python.el --- Configures `python-mode'

;; Copyright (C) 2016-2022  Miguel Guedes

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
;; Install the following PIP packages to ensure that elpy and flycheck work as expected:
;;
;; # sudo pip install jedi pylint flake8 autopep8
;;
;; Note that fake8 is invoked by pylint (C-c C-v) whereas pylint is used by flycheck.

;;

;;; Code:

;; Defaults
(setq-default python-indent-offset 4)

(defun init/python ()
  "Python mode load hook.")

(defun init/python-mode ()
  "Customise `python-mode'."

  (setq-local fill-column init/defaults/fill-column/narrow)
  ;; Undoing replacement of projectile's key bindings
  (define-key elpy-mode-map (kbd "C-c C-p") nil)
  (define-key elpy-mode-map (kbd "C-c C-n") nil)
  (py-autopep8-enable-on-save))

(use-package python-mode
  :hook ((python-mode . init/common-nonweb-programming-mode)
         (python-mode . pylint-add-menu-items)
         (python-mode . pylint-add-key-bindings)
         (python-mode . init/python-mode)))

(use-package pylint
  :after python-mode)

;; Installation instructions straight from:
;; https://elpy.readthedocs.io/en/latest/introduction.html#installation
(use-package elpy
  :after python-mode
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))


;;; python.el ends here
