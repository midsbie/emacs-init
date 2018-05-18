;;; lisp.el --- Configures `lisp-mode'

;; Copyright (C) 2015-2018  Miguel Guedes

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

(eval-after-load 'lisp-mode
  '(progn
     (add-hook 'lisp-mode-hook             'init/common-programming)
     (add-hook 'lisp-mode-hook             'init/elisp)

     (add-hook 'emacs-lisp-mode-hook       'init/common-programming)
     (add-hook 'emacs-lisp-mode-hook       'init/elisp)

     (add-hook 'lisp-interaction-mode-hook 'init/common-programming)
     (add-hook 'lisp-interaction-mode-hook 'init/elisp)))

;;; lisp.el ends here
