;;; init-python.el --- Configures `python-mode'  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2026  Miguel Guedes

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
;; Ruff:
;; `ruff' lints and formats Python, replacing flake8, pyflakes, Black and other
;; tools.
;;
;; Projects that format and lint with ruff opt in from the .dir-locals.el at
;; their root:
;;
;;   ((python-base-mode
;;     . ((eval . (ruff-format-on-save-mode))
;;        (init/inhibit-buffer-formatting . t)
;;        (eglot-workspace-configuration
;;         . (:pylsp (:plugins (:pycodestyle (:enabled :json-false))))))))
;;
;; - Without the opt-in, saving formats through pylsp's autopep8 at
;;   pycodestyle's 79 columns, which fights ruff's own line length.
;; - pylsp enables pycodestyle by default, duplicating ruff's E501 at 79.
;;   Eglot reads `eglot-workspace-configuration' at the project root, so it has
;;   no effect inside a subdirectory ("dir/") entry.
;; - `pet' (features/init-pet.el) points `ruff-format' and `flymake-ruff' at the
;;   project's own ruff.
;;
;; Why an opt-in rather than detection: nothing standard declares a project's
;; formatter.  [tool.ruff] often configures linting only, next to Black for
;; formatting; [tool.ruff.format] is optional; and the real choice may live only
;; in a justfile, Makefile or CI script.  The cost of guessing wrong is
;; lopsided: wrongly guessing ruff reformats whole files of someone else's
;; project on the first save, while wrongly guessing nothing merely leaves the
;; buffer unformatted.  Editors make format-on-save an explicit per-project
;; setting for the same reason, e.g. VS Code's `editor.formatOnSave' in
;; .vscode/settings.json.
;;
;; Language server:
;; Eglot starts pylsp itself, so it only needs installing.  `pet' prefers a
;; pylsp in the project's virtualenv over the one on PATH, such as a
;; distribution package like Debian's python3-pylsp; system Pythons often refuse
;; pip installs (PEP 668).

;;; Code:

(defun init/python/enable ()
  "Customise `python-mode'."

  (init/common-nonweb-programming-mode)
  ;; Ruff's and Black's default; PEP 8 uses 79 for code and 72 for comments.
  (setq-local fill-column 88))

(use-package python
  :ensure nil
  :mode (("\\.py\\'" . python-ts-mode))
  :interpreter (("python[0-9.]*" . python-ts-mode))
  :hook ((python-mode python-ts-mode) . init/python/enable))

(provide 'init-python)

;;; init-python.el ends here
