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
    ;; keeping following configuration here for posteriority.
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
        (add-to-list 'eglot-server-programs `(,mode . ,(cdr server-program))))))
  (advice-add 'eglot-rename :around #'init/eglot/rename-advice)
  (advice-add 'eglot-uri-to-path :around #'init/eglot/uri-to-path-advice)
  (advice-add 'eglot--TextDocumentIdentifier :around #'init/eglot/TextDocumentIdentifier))

(defun init/eglot/enable ()
  "Configure `eglot' when enabled in a buffer."
  (add-hook 'before-save-hook #'init/maybe-format-buffer nil t))

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

(defun init/eglot/rename-advice (orig-fun &rest args)
  "Advice to pre-fill the current symbol name in `eglot-rename' prompt."
  (interactive
   (let ((current-symbol (thing-at-point 'symbol t)))
     (list (read-from-minibuffer
            (format "Rename `%s' to: " (or current-symbol "unknown symbol"))
            (symbol-name (symbol-at-point)))))
  )
  (apply orig-fun args))

(defun init/eglot/special-uri-to-path (uri)
  "Convert PATH, a file name, to LSP URI string and return it.
TRUENAMEP indicated PATH is already a truename."
  (cond
   ((string-prefix-p "csharp:/" uri)
    (init/eglot/csharp-cls-metadata-uri-handler uri))
   (t
    uri)))

(defun init/eglot/uri-to-path-advice (orig-fun uri)
  "Custom function to handle special URIs in Eglot."
  (funcall orig-fun (init/eglot/special-uri-to-path uri)))

(defun init/eglot/csharp-cls-metadata-uri-handler (uri)
  "Handle `csharp:/(metadata)' URI from csharp-ls server in Eglot.

A cache file is created in the project root that stores this metadata,
and the filename is returned so Eglot can display the file.

Function mostly lifted from lsp-charp.el."
  (let* ((workspace (eglot--current-server-or-lose))
         (metadata-req (list :textDocument (list :uri uri)))
         (metadata (jsonrpc-request workspace "csharp/metadata" metadata-req))
         ;; Access metadata fields using plist-get
         (project-name (plist-get metadata :projectName))
         (assembly-name (plist-get metadata :assemblyName))
         (symbol-name (plist-get metadata :symbolName))
         (source (plist-get metadata :source))
         (filename (concat ".cache/lsp-csharp/metadata/projects/" project-name
                           "/assemblies/" assembly-name "/" symbol-name ".cs"))
         (file-location (expand-file-name filename (project-root (eglot--current-project))))
         (metadata-file-location (concat file-location ".metadata-uri"))
         (path (file-name-directory file-location)))

    (unless (file-exists-p file-location)
      (unless (file-directory-p path)
        (make-directory path t))

      (with-temp-file metadata-file-location
        (insert uri))

      (with-temp-file file-location
        (insert source)))

    file-location))

(defun init/eglot/TextDocumentIdentifier (orig-fun &rest args)
  "Advice for `eglot--TextDocumentIdentifier`.

This function initialized `eglot--TextDocumentIdentifier-cache' with the
true URI of the active buffer.  Doing so makes it possible to visit
decompiled C# assemblies without triggering errors such as the following:

The type 'GameObject' exists in both 'Unity.Timeline, Version=1.0.0.0,
Culture=neutral, PublicKeyToken=null' and 'UnityEngine.CoreModule,
Version=0.0.0.0, Culture=neutral, PublicKeyToken=null'
"
  (unless eglot--TextDocumentIdentifier-cache
    (when-let ((metadata-uri (init/eglot/determine-true-buffer-uri)))
      (setq-local eglot--TextDocumentIdentifier-cache
            `(,buffer-file-name . (:uri ,metadata-uri :truenamep t)))))
  (apply orig-fun args))

(defun init/eglot/determine-true-buffer-uri ()
  "Determine the true URI of the buffer.

This function determines the URI that should be communicated to the LSP
server.  Currently this is only valid for C# scripts for which a file
exists with the suffix .metadata-uri file; in this particular scenario,
the contents of the metadata file is returned.  In all other cases, nil
is returned."
  (let ((metadata-file-name (concat buffer-file-name ".metadata-uri")))
    (when (file-exists-p metadata-file-name)
      (with-temp-buffer (insert-file-contents metadata-file-name)
                        (buffer-string)))))

(use-package eglot
  :config (init/eglot/config)
  :hook (eglot-managed-mode . init/eglot/enable)

  :bind (:map eglot-mode-map
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
