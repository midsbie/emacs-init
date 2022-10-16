;;; typescript.el --- Configures `typescript-mode'

;; Copyright (C) 2020-2022  Miguel Guedes

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
;; 22-06-05 Transitioned to the amazing eglot package
;; 21-11-17 Moving back to TIDE as LSP is too slow
;; 21-09-03 Disabling TIDE in favour of LSP

;;; Code:

;; Unclear why but it isrequired to prevent error when removed/not present.
(cl-defmethod project-root ((project (head eglot-project)))
  (cdr project))

(defun init/typescript/try-tsconfig-json (dir)
  "Locate the tsconfig.json file in a sub-tree of DIR.

Successfully locating Typescript's configuration file means that
we are indeed in a Typescript repository.

More importantly this function allows for `eglot' to be
configured correctly for monorepositories."
  (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
    (cons 'eglot-project found)))

(defun init/typescript/config/ts-tsx ()
  "Configure buffer for Typescript development."
  (init/common-web-programming-mode)
  (setq-local typescript-indent-level 2)

  (local-set-key (kbd "M-a") 'c-beginning-of-statement)
  (local-set-key (kbd "M-e") 'c-end-of-statement)

  ;; (init/typescript/config/tide)
  (eglot-ensure)
  (flymake-eslint-enable))

(defun init/typescript/config/tide ()
  "Enable TIDE in Typescript buffer."
  (interactive)
  (tide-setup)

  (setq-local flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq-local company-tooltip-align-annotations t)

  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint)
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)

  (eldoc-mode 1)
  (tide-hl-identifier-mode 1))

(use-package typescript-mode
  :diminish "TS"
  :mode ("\\.ts\\'")

  :hook ((typescript-mode . init/typescript/config/ts-tsx))

  :config
  ;; This is required to ensure Eglot is configured correctly for monorepository
  ;; projects.
  (add-hook 'project-find-functions
            'init/typescript/try-tsconfig-json nil nil)
  )

;;; typescript.el ends here
