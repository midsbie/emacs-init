;;; eglot.el --- Customises the Eglot package

;; Copyright (C) 2022  Miguel Guedes

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

(defvar init/typescript-server-location
  (expand-file-name
   "~/.emacs.d/.cache/lsp/npm/typescript-language-server/bin/typescript-language-server"))

(defvar init/eglot/extra-server-programs
  `(
    (vala-mode . ("vala-language-server"))
    (typescript-mode . (,init/typescript-server-location "--stdio"))
    (tsx-ts-mode . (,init/typescript-server-location "--stdio"))
    (typescript-ts-mode . (,init/typescript-server-location "--stdio"))))

(defun init/eglot ()
  "Configure `eglot' package."

  (dolist (server-program init/eglot/extra-server-programs)
    (let ((mode (car server-program))
          (args (cdr server-program)))
      (unless (assoc mode eglot-server-programs)
        (add-to-list 'eglot-server-programs `(,mode . ,args)))))

  ;; Configure `eglot' to support Typescript source files when edited in
  ;; `web-mode'.
  ;;
  ;; This is currently disabled because web-mode is no longer used for this
  ;; purpose.
  ;;
  ;;   (if (assoc 'web-mode eglot-server-programs)
  ;;       (setcdr (assoc 'web-mode eglot-server-programs)
  ;;               '(,init/typescript-server-location "--stdio"))
  ;;     (add-to-list 'eglot-server-programs
  ;;                  '(web-mode . (,init/typescript-server-location "--stdio"))))
  )

(defun init/eglot/config ()
  "Configure `eglot' when enabled in a buffer."
  ;; This may prove problematic for some modes and may need to be disabled or
  ;; enabled conditionally on a per-mode basis.  If conditional configuration is
  ;; required, please make sure to keep configuration centralized in this
  ;; module.
  (add-hook 'before-save-hook 'eglot-format-buffer nil t)
)

(use-package eglot
  :hook (eglot-managed-mode . init/eglot/config)

  :bind (
         :map eglot-mode-map
              ("C-c l a a" . eglot-code-actions)
              ("C-c l r r" . eglot-rename)
              )

  :config (init/eglot))

;;; eglot.el ends here
