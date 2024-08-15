;;; eglot.el --- Customises the Eglot package

;; Copyright (C) 2022-2024  Miguel Guedes

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

;; This tutorial documents a user's journey to Eglot from LSP:
;; https://andreyor.st/posts/2023-09-09-migrating-from-lsp-mode-to-eglot/

;;; Code:

(defvar init/typescript-server-location
  (expand-file-name
   "~/.emacs.d/.cache/lsp/npm/typescript-language-server/bin/typescript-language-server"))

(defvar init/eglot/extra-server-programs
  `(
    ;; All Typescript-related modes are now supported natively by Eglot but
    ;; following configuration here for posteriority.
    (tsx-ts-mode . (,init/typescript-server-location "--stdio"))
    (typescript-mode . (,init/typescript-server-location "--stdio"))
    (typescript-ts-mode . (,init/typescript-server-location "--stdio"))
    (vala-mode . ("vala-language-server"))))

(defun init/eglot/config ()
  "Configure `eglot' package."

  (dolist (server-program init/eglot/extra-server-programs)
    (let ((mode (car server-program)))
      (unless (init/eglot/server-program-supported-p mode)
        (message "adding eglot support for %s" mode)
        (add-to-list 'eglot-server-programs `(,mode . ,(cdr server-program)))))))

(defun init/eglot/enable ()
  "Configure `eglot' when enabled in a buffer."
  (unless (memq major-mode init/format-buffer-on-save-mode-exclusions)
    (add-hook 'before-save-hook #'(lambda ()
                                    (ignore-errors
                                      (eglot-format-buffer))) nil t)))

(defun init/eglot/server-program-supported-p (major-mode)
  "Check if the given MAJOR-MODE is supported by `eglot-server-programs'."
  (let ((supported nil))
    (dolist (entry eglot-server-programs supported)
      (let ((mode (car entry)))
        (cond
         ;; If mode is a list, check if major-mode is an element of this list
         ((listp mode)
          (dolist (submode mode)
            (cond
             ;; Check if submode is a list that might contain a major mode and metadata
             ((listp submode)
              (when (eq major-mode (car submode)) (setq supported t)))
             ;; Check if submode is a symbol that matches major-mode
             ((eq major-mode submode) (setq supported t)))))
         ;; If mode is a symbol, check if it matches major-mode
         ((eq major-mode mode) (setq supported t))
         )))))

(use-package eglot
  :config (init/eglot/config)
  :hook (eglot-managed-mode . init/eglot/enable)

  :bind (
         :map eglot-mode-map
              ("C-c l a a" . eglot-code-actions)
              ("C-c l r r" . eglot-rename)
              )

  ;; Most the customizations below were inspired by the tutorial referenced in
  ;; the Commentary section.
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref nil)
  ;; Other features one might consider disabling:
  ;;
  ;; * :hoverProvider
  ;;   :documentHighlightProvider
  ;;
  ;;    The latter capability seems to require the former.
  ;;
  ;; * :documentFormattingProvider
  ;;   :documentRangeFormattingProvider
  ;;
  ;;   There does not seem to be any point in disabling these capabilities
  ;;   because they have to be explicitly invoked by the user; i.e. they don't
  ;;   run automatically on save.  Disabling them means `eglot-format-buffer'
  ;;   will not work.
  (eglot-ignored-server-capabilities
   '(
     :documentOnTypeFormattingProvider
     :colorProvider
     :foldingRangeProvider))
  (eglot-stay-out-of '(yasnippet)))

;;; eglot.el ends here
