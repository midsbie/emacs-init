;;; init-pet.el --- Configure pet  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `pet' finds each Python project's virtualenv (pixi, conda, poetry, hatch,
;; pipenv, a `pet-venv-dir-names' directory such as .venv, pyenv) and points
;; Python tooling at its executables through buffer-local variables, so no
;; virtualenv is ever activated globally.  It configures `ruff-format', eglot
;; and `python-shell-interpreter' itself; `flymake-ruff' is wired in
;; features/init-flymake-ruff.el.
;;
;; A VIRTUAL_ENV inherited from the shell that started Emacs takes precedence
;; over discovery, for every project.

;;; Code:

(use-package pet
  :hook (python-base-mode . pet-mode))

(provide 'init-pet)

;;; init-pet.el ends here
