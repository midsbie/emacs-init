;;; init-lsp-mode.el --- Configures the `lsp' package  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026  Miguel Guedes

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

;;; Installation:
;;
;; PERFORMANCE
;;
;; This article contains super important tips for resolving performance
;; problems or simply improving the performance of LSP and related modes:
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;;
;; Note that support for plists (property lists) should NOT be enabled as this
;; feature was found to significantly degrade performance when editing
;; Dart/Flutter source files.  See inline comment further below.
;;
;; TYPESCRIPT
;; 1. Install `typescript-language-server` for javascript and typescript
;;    support, as per documentation at:
;;    https://github.com/typescript-language-server/typescript-language-server
;;
;;    $ npm install -g typescript-language-server
;;
;; CSHARP
;; 1. Install the `mono-devel` system package by following the instructions on
;;    Mono's website: https://www.mono-project.com/download/stable/
;;
;; 2. Install the omnisharp-roslyn LSP server:
;;    M-x lsp-install-server RET csharp RET
;;
;; 3. The Omnisharp Roslyn server bundles its own minimal Mono runtime,
;;    libraries and will prevent code instrospection and other LSP features
;;    from working as expected.  Fix this by symlinking to the system's Mono
;;    packages:
;;    $ cd ~/.emacs.d/.cache/lsp/omnisharp-roslyn/latest/omnisharp-roslyn/lib
;;    $ rm -rf mono
;;    $ ln -s /usr/lib/mono

;;; Commentary:

;; This tutorial documents a user's journey to Eglot from LSP but also contains
;; a great deal of information on how to set up the latter:
;; https://andreyor.st/posts/2023-09-09-migrating-from-lsp-mode-to-eglot/

;;; Code:

(defun init/lsp-mode/config ()
  "Initialise LSP."
  ;; Enable plists when the required environment var is set to true.
  ;;
  ;; Note that an experiment was conducted to ascertain the possibility of
  ;; improving performance of parsing of LSP messages but the verdict was that
  ;; it significantly worsened performance when editing Dart/Flutter source
  ;; files, and also generally other modes too though to a lesser extent.
  ;;
  ;; Ref: https://emacs-lsp.github.io/lsp-mode/page/performance/
  (when (string= (getenv "LSP_USE_PLISTS") "true")
    (warn "Warning: LSP_USE_PLISTS environment var set: performance likely
to degrade under LSP")))

(defun init/lsp-mode/enable ()
  (lsp-enable-which-key-integration)

  ;; This can't be in the initializing defun above or it'll error out.
  ;; Ref: https://github.com/emacs-lsp/lsp-mode/issues/1532#issuecomment-602384182
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (add-hook 'before-save-hook #'init/maybe-format-buffer nil t))

(defun init/lsp-mode/after-open-hook ()
  "Set up buffer after connection to LSP.

This function is used as a hook to perform certain actions after a
buffer is opened.  One of the actions it performs is to check the buffer
using Flycheck since, for some reason, this is not done by default in
Typescript buffers and possibly other modes too.  Note that a delay of 1
second is enforced otherwise flycheck fails to check the
buffer (observed in `typescript-ts-mode'."
  (run-with-idle-timer 1 nil #'(lambda ()
                                 (ignore-errors
                                   (when (and (boundp 'flycheck-mode) flycheck-mode)
                                     (flycheck-buffer))))))

(defun my/lsp/log-request (type method)
  "Log LSP request.
Prints TYPE and METHOD to special 'lsp-output' buffer.  Meant to
be used when debugging `lsp'."
  (with-current-buffer "lsp-output"
    (goto-char (point-min))
    (insert (format "[%s] <%s %s\n" (format-time-string "%H:%M:%S:%N" (current-time))
                    type
                    method))
    (goto-char (point-min))))

;; Don't specify modes or file extensions here that LSP should initialize
;; against.  The form in use calls for `lsp-deferred' to be invoked in the
;; major mode configuration module; e.g. typescript.el, csharp.el.
(use-package lsp-mode
  :custom
  (lsp-log-io nil)
  (lsp-keep-workspace-alive nil)
  (lsp-auto-guess-root t)
  (lsp-idle-delay 0.5)
  (lsp-enable-symbol-highlighting t)
  ;; Lens annotations have failed to update and interfered with selections.
  (lsp-lens-enable nil)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-eldoc-enable-hover t)
  (lsp-eldoc-render-all t)
  (lsp-signature-auto-activate '(:on-trigger-char :after-completion :on-server-request))
  (lsp-signature-render-documentation t)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable t)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-completion-show-detail t)
  (lsp-completion-show-kind t)
  :config (init/lsp-mode/config)
  :hook ((lsp-mode . init/lsp-mode/enable)
         (lsp-after-open . init/lsp-mode/after-open-hook)))

(provide 'init-lsp-mode)

;;; init-lsp-mode.el ends here
