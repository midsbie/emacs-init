;;; python.el --- Configures `python-mode'

;; Copyright (C) 2016-2024  Miguel Guedes

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
;; 070423 elpy and python-mode packages removed in favor of the native python
;;        and tree-sitter packages.
;;
;; ## INSTALLATION
;; ### LINTERS
;; Install the following PIP packages to ensure that elpy and flycheck work as
;; expected:
;;
;; # pip install jedi pylint flake8 autopep8
;;
;; Note that flake8 is invoked by pylint (C-c C-v) whereas pylint is used by
;; `flycheck'.  Beware that if using `eglot', `flymake' will be in use instead
;; and may therefore not enable pylint.

;; ### RUNNING EGLOT OR LSP CLIENTS
;;
;; For `eglot' or`lsp' to activate one needs to:
;;
;; 1. Install Python package:
;;    pip install python-lsp-server[all]
;;
;; 2. Run `pylsp`:
;;    pylsp
;;
;; Additional information at:
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-pylsp/

;; ## PACKAGE ELPY NOT ENABLED
;;
;; The elpy-mode is currently not enabled because it was found to be super
;; annoying.  Main reason is it defines keybindings that clash with existing
;; ones.  Might be possible to customize but unclear if worth the effort.
;;
;; Installation instructions straight from:
;; https://elpy.readthedocs.io/en/latest/introduction.html#installation

;;

;;; Code:

(defun init/python-mode/enable ()
  "Customise `python-mode'."

  (init/common-nonweb-programming-mode)
  (pylint-add-menu-items)
  (pylint-add-key-bindings)

  (setq-local fill-column init/defaults/fill-column/narrow)

  (py-autopep8-mode)
  (auto-fill-mode -1))

(use-package python
  :mode (("\\.py\\'" . python-ts-mode))
  :hook ((python-mode python-ts-mode) . init/python-mode/enable))

(use-package pylint
  :after python-mode)

;;; python.el ends here
