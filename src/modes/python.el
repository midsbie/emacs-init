;;; python.el --- Configures `python-mode'

;; Copyright (C) 2016-2018  Miguel Guedes

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

(add-hook 'python-load-hook 'init/python)

;; Defaults
(setq-default python-indent-offset 4)

(defun init/python ()
  "Python mode load hook."
  (load "pylint")

  (add-hook 'python-mode-hook 'init/common-programming)
  (add-hook 'python-mode-hook 'init/python-mode)
  (add-hook 'python-mode-hook 'pylint-add-menu-items)
  (add-hook 'python-mode-hook 'pylint-add-key-bindings))

(defun init/python-mode ()
  "Customise `python-mode'."

  (py-autopep8-enable-on-save)

  ;; The following doesn't work to ensure C-j does the right thing.
  (define-key python-mode-map (kbd "C-j") 'newline-and-indent)
  (define-key python-mode-map (kbd "RET") 'newline-indent)

  ;; ... so we're having to resort to using a hack.
  (run-at-time "1 sec" nil
    '(lambda()
      (local-set-key (kbd "C-j") 'newline-and-indent)
      (local-set-key (kbd "RET") 'newline)))
)

;;; python.el ends here
