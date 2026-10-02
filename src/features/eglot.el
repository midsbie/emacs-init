;;; eglot.el --- Customises the Eglot package

;; Copyright (C) 2022-2026  Miguel Guedes

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

(defgroup init/eglot nil
  "Customisations for `eglot'."
  :group 'init
  :prefix "init/eglot")

(defcustom init/eglot/inlay-hints-enabled t
  "Whether `eglot-inlay-hints-mode' should be enabled.
This variable is persisted across sessions via the Customize
framework and is updated automatically when toggling inlay hints
with `init/eglot/toggle-inlay-hints'."
  :group 'init/eglot
  :type 'boolean)

(defun init/eglot/config ()
  "Configure `eglot' package."
  ;; Eglot has no built-in entry for Vala.
  (add-to-list 'eglot-server-programs '(vala-mode . ("vala-language-server")))

  ;; Completely disable the events buffer for maximum performance.  Eglot keeps
  ;; 2000000 events by default, which are suspected to cause performance issues
  ;; when editing Dart files that are part of a Flutter project.
  ;;
  ;; NOTE: This setting WILL make debugging Eglot issues more difficult and
  ;; isn't necessary in all cases observed.  Disabling the events buffer should
  ;; be a project-specific override done via .dir-locals.el and NEVER globally.
  ;; Keeping the line below here for future reference.
  ;;
  ;; (setq eglot-events-buffer-config '(:size 0 :format nil))
  ;;
  ;; If performance is still a concern in some modes, consider sending edits
  ;; less frequently:
  ;;
  ;; (setq eglot-send-changes-idle-time 1.0)

  ;; Configure Typescript language server with inlay hints enabled by default.
  (add-to-list
   'eglot-server-programs
   `((typescript-ts-mode tsx-ts-mode js-ts-mode typescript-mode js-mode)
     . ("typescript-language-server" "--stdio"
        :initializationOptions
        (:preferences
         (:includeInlayParameterNameHints "all"
                                          :includeInlayParameterNameHintsWhenArgumentMatchesName t
                                          :includeInlayFunctionParameterTypeHints t
                                          :includeInlayVariableTypeHints t
                                          :includeInlayVariableTypeHintsWhenTypeMatchesName t
                                          :includeInlayPropertyDeclarationTypeHints t
                                          :includeInlayFunctionLikeReturnTypeHints t
                                          :includeInlayEnumMemberValueHints t)))))

  ;; csharp-ls answers go-to-definition on symbols in compiled assemblies (BCL,
  ;; NuGet) with an empty result unless `metadata-uris' is enabled; with it, the
  ;; server returns `csharp:/' URIs, resolved by `init/eglot/uri-to-path-advice'.
  ;; `add-to-list' prepends, so this entry shadows Eglot's built-in C# one.
  (add-to-list 'eglot-server-programs
               '((csharp-mode csharp-ts-mode) . ("csharp-ls" "--features" "metadata-uris")))

  (add-hook 'eglot-managed-mode-hook #'init/eglot/enable t)
  (advice-add 'eglot-rename :around #'init/eglot/rename-advice)
  (advice-add 'eglot-uri-to-path :around #'init/eglot/uri-to-path-advice)
  (advice-add 'eglot-path-to-uri :around #'init/eglot/path-to-uri-advice)
  (add-hook 'find-file-hook #'init/eglot/csharp-metadata-read-only))

(defun init/eglot/enable ()
  "Set up the current buffer as `eglot' starts or stops managing it.
`eglot-managed-mode-hook' runs on both transitions, so undo the setup
once the buffer is no longer managed."
  (if (eglot-managed-p)
      (progn
        (unless init/eglot/inlay-hints-enabled
          (eglot-inlay-hints-mode -1))
        (add-hook 'before-save-hook #'init/maybe-format-buffer nil t))
    (remove-hook 'before-save-hook #'init/maybe-format-buffer t)))

(defun init/eglot/insert-minibuffer-default ()
  "Insert the minibuffer's default value as editable input."
  (when (stringp minibuffer-default)
    (insert minibuffer-default)))

(defun init/eglot/rename-advice (orig-fun &rest args)
  "Pre-fill the `eglot-rename' prompt with the name being renamed.
Eglot offers the name as the minibuffer default; insert it instead.  The
original interactive spec still runs, so the server's `prepareRename'
check and suggested name are kept.  ORIG-FUN is called with ARGS."
  (interactive
   (lambda (spec)
     (minibuffer-with-setup-hook #'init/eglot/insert-minibuffer-default
       (advice-eval-interactive-spec spec))))
  (apply orig-fun args))

(defun init/eglot/uri-to-path-advice (orig-fun uri)
  "Resolve csharp-ls `csharp:/' URIs to cache files; pass others to ORIG-FUN."
  (if (and (stringp uri) (string-prefix-p "csharp:/" uri))
      (init/eglot/csharp-metadata-file uri)
    (funcall orig-fun uri)))

(defvar init/eglot/csharp-metadata-files (make-hash-table :test #'equal)
  "Cache files written this session, keyed by csharp-ls `csharp:/' URI.
Eglot resolves a URI once per location in a result, so this saves a
`csharp/metadata' round trip for each.  Starting empty every session means
a file left behind by an older csharp-ls or SDK is rewritten on first use.")

(defun init/eglot/csharp-metadata-file (uri)
  "Return the cache file holding the decompiled source for URI."
  (let ((file (gethash uri init/eglot/csharp-metadata-files)))
    (if (and file (file-exists-p file))
        file
      (puthash uri (init/eglot/csharp-fetch-metadata uri)
               init/eglot/csharp-metadata-files))))

(defun init/eglot/csharp-fetch-metadata (uri)
  "Write the source for URI, fetched via `csharp/metadata', to the cache.
The file lives under the project root next to a sidecar recording URI
\(see `init/eglot/csharp-metadata-uri-file'); its name is returned."
  (let* ((server (or (eglot-current-server)
                     (user-error "No Eglot server to resolve %s" uri)))
         (metadata (jsonrpc-request server "csharp/metadata" `(:textDocument (:uri ,uri))))
         (source (or (plist-get metadata :source)
                     (user-error "csharp-ls returned no source for %s" uri)))
         (file (expand-file-name
                (format ".cache/lsp-csharp/metadata/projects/%s/assemblies/%s/%s.cs"
                        (plist-get metadata :projectName)
                        (plist-get metadata :assemblyName)
                        (plist-get metadata :symbolName))
                (project-root (project-current t)))))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert source))
    (with-temp-file (init/eglot/csharp-metadata-uri-file file) (insert uri))
    file))

(defun init/eglot/csharp-metadata-uri-file (file)
  "Return the sidecar recording the `csharp:/' URI FILE was fetched from."
  (concat file ".metadata-uri"))

(defun init/eglot/csharp-metadata-uri (file)
  "Return the `csharp:/' URI FILE was fetched from, or nil if it wasn't."
  (let ((sidecar (init/eglot/csharp-metadata-uri-file file)))
    (when (file-exists-p sidecar)
      (with-temp-buffer
        (insert-file-contents sidecar)
        (buffer-string)))))

(defun init/eglot/csharp-metadata-read-only ()
  "Make a buffer visiting a decompiled C# cache file read-only.
Edits could never reach the assembly, and would desync the buffer from the
source csharp-ls holds for its `csharp:/' URI."
  (when (and buffer-file-name
             (file-exists-p (init/eglot/csharp-metadata-uri-file buffer-file-name)))
    (read-only-mode 1)))

(defun init/eglot/path-to-uri-advice (orig-fun path &rest args)
  "Address decompiled C# cache files by their `csharp:/' URI.
csharp-ls tracks decompiled documents only by that URI, so requests made
from a buffer visiting the cache file under its `file:' URI find nothing.
Other paths go to ORIG-FUN with ARGS."
  (or (init/eglot/csharp-metadata-uri path)
      (apply orig-fun path args)))

(defun init/eglot/toggle-inlay-hints (&optional global)
  "Toggle `eglot-inlay-hints-mode' in the current buffer.
With prefix argument GLOBAL, also set all other eglot-managed buffers
to the same state."
  (interactive "P")
  (eglot-inlay-hints-mode 'toggle)
  (customize-save-variable 'init/eglot/inlay-hints-enabled eglot-inlay-hints-mode)
  (when global
    (let ((state (if eglot-inlay-hints-mode 1 -1))
          (current (current-buffer)))
      (dolist (buf (buffer-list))
        (unless (eq buf current)
          (with-current-buffer buf
            (when (eglot-managed-p)
              (eglot-inlay-hints-mode state)))))))
  (message "Inlay hints %s%s"
           (if eglot-inlay-hints-mode "enabled" "disabled")
           (if global " globally" "")))

(use-package eglot
  :config (init/eglot/config)
  ;; NOTE: the hook is appended via `init/eglot/config' so that it runs AFTER
  ;; eglot's own default hooks (which auto-enable `eglot-inlay-hints-mode').
  ;; Using :hook here would prepend, causing our state to be overridden.

  :bind (:map eglot-mode-map
              ("C-c l a a" . eglot-code-actions)
              ("C-c l r r" . eglot-rename)
              ("C-c l t h" . init/eglot/toggle-inlay-hints)
              )

  ;; Most the customizations below were inspired by the tutorial referenced in
  ;; the Commentary section.
  :custom
  (eglot-autoshutdown t)
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
