;;; typescript.el --- Configures `typescript-mode'

;; Copyright (C) 2020-2024  Miguel Guedes

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
;; Configuration of `eglot' by locating the `tsconfig.json' file found at:
;; https://notes.alexkehayias.com/setting-up-typescript-and-eslint-with-eglot/

;;; Log:
;;
;; 260323 Reverted to using `lsp' as an experiment to solve weird performance
;;        issues under eglot.
;;
;; 050622 Transitioned to the amazing `eglot' package
;;
;; 171121 Moving back to TIDE as LSP is too slow
;;
;; 030921 Disabling TIDE in favour of LSP

;;; Code:

;; Unclear why but it isrequired to prevent error when removed/not present.
(cl-defmethod project-root ((project (head eglot-project)))
  (cdr project))

(defun init/typescript/config ()
  "Configure Typescript-related modes."
  (when (eq (init/get-language-server major-mode) 'eglot)
    ;; This is required to ensure Eglot is configured correctly for monorepository
    ;; projects.
    (add-hook 'project-find-functions
              'init/typescript/try-tsconfig-json nil nil)))

(defun init/typescript/try-tsconfig-json (dir)
  "Locate the tsconfig.json file in a sub-tree of DIR.

Successfully locating Typescript's configuration file means that
we are indeed in a Typescript repository.

More importantly this function allows for `eglot' to be
configured correctly for monorepositories."
  ;; This no longer seems to be required to get Eglot to work with
  ;; monorepositories.
  ;; ---
  ;;   (when-let* ((found (my/locate-topmost-file "tsconfig.json" dir)))
  ;;     (cons 'eglot-project found)))
  nil)

(defun init/typescript/enable ()
  "Configure buffer for Typescript development."
  (setq-local typescript-ts-mode-indent-offset 2)
  (setq-local lsp-eslint-enable nil)
  (setq-local lsp-eslint-run "onSave")
  (setq-local lsp-eslint-format nil)

  (init/common-web-programming-mode)

  ;; Adding 'operator to level 4 font lock features
  (push 'operator (car (last treesit-font-lock-feature-list)))
  (treesit-major-mode-setup))

(use-package typescript-ts-mode
  :diminish "TS"
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :hook ((typescript-ts-mode tsx-ts-mode) . init/typescript/enable)
  :config (init/typescript/config))

;;; typescript.el ends here
