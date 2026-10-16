;;; pet.el --- Configures `pet'

;; Copyright (C) 2026  Miguel Guedes

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
;; `pet' finds each Python project's virtualenv (pixi, conda, poetry, hatch,
;; pipenv, a `pet-venv-dir-names' directory such as .venv, pyenv) and points
;; Python tooling at its executables through buffer-local variables, so no
;; virtualenv is ever activated globally.  It configures `ruff-format', eglot
;; and `python-shell-interpreter' itself; `flymake-ruff' is wired in
;; features/flymake.el.
;;
;; A VIRTUAL_ENV inherited from the shell that started Emacs takes precedence
;; over discovery, for every project.

;;; Code:

(use-package pet
  :ensure t
  :hook (python-base-mode . pet-mode))

;;; pet.el ends here
