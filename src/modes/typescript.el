;;; typescript.el --- Configures Typescript buffers

;; Copyright (C) 2020-2026  Miguel Guedes

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

;; Eglot may fail to start if the TypeScript server (`tsserver`) is not found in
;; the default location (`./node_modules/.bin`) relative to the project's root
;; directory. To resolve this issue, you need to customize the command that
;; Eglot uses to start the server by specifying the path to `tsserver` manually
;; in the `tsserver.path` option under `initializationOptions`.
;;
;; The recommended approach is to create a `.dir-locals.el` file in your project
;; root with the following configuration:
;;
;; (((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode)
;;   . ((eglot-server-programs
;;       . ((((js-mode :language-id "javascript")
;;            (js-ts-mode :language-id "javascript")
;;            (tsx-ts-mode :language-id "typescriptreact")
;;            (typescript-ts-mode :language-id "typescript")
;;            (typescript-mode :language-id "typescript"))
;;           . ("typescript-language-server" "--stdio"
;;              :initializationOptions
;;              (:tsserver (:path "./server/node_modules/.bin/tsserver")))))))))

;;; Code:

(defun init/typescript/enable ()
  "Configure buffer for Typescript development."
  (setq-local lsp-eslint-enable nil)
  (setq-local lsp-eslint-run "onSave")
  (setq-local lsp-eslint-format nil)

  (init/common-web-programming-mode)

  ;; `tsx-ts-mode' leaves `operator' out of its feature levels.  Enable it for
  ;; this buffer only; the feature list itself is shared by all buffers.
  (when (treesit-parser-list)
    (treesit-font-lock-recompute-features '(operator))))

(use-package typescript-ts-mode
  :diminish "TS"
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :hook ((typescript-ts-mode tsx-ts-mode) . init/typescript/enable))

;;; typescript.el ends here
