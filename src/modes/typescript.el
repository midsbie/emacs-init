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
;; ??0824 Transitioned again to the `eglot' package as it is now a core Emacs
;;        package and works much better than `lsp'.
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

;; Required to prevent an error when removed/not present.
(cl-defmethod project-root ((project (head eglot-project)))
  (cdr project))

(defun init/typescript/config ()
  "Configure Typescript-related modes."
  (when (eq (init/get-language-server major-mode) 'eglot)
    (add-hook 'project-find-functions
              'init/typescript/try-tsconfig-json nil nil)))

(defun init/typescript/try-tsconfig-json (dir)
  "Search for the nearest tsconfig.json file in a subdirectory of DIR.

This function helps identify if the current directory is part of a
Typescript project by locating a 'tsconfig.json' file within its
subtree.  If found, it returns a configuration suitable for the `eglot'
LSP client.

This is particularly useful for handling monorepositories with complex
structures, which may have one or more of the following characteristics:

a) Multiple 'tsconfig.json' files due to the presence of multiple packages.

b) A single 'tsconfig.json' file that is not located at the repository's root.
"
  (when-let* ((found (my/locate-topmost-file "tsconfig.json" dir)))
    (cons 'eglot-project found)))

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
