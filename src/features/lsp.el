;;; lsp.el --- Configures the `lsp' package

;; Copyright (C) 2021-2022  Miguel Guedes

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
;; Note that support for plists should NOT be enabled as this feature was found
;; to significantly degrade performance when editing Dart/Flutter source files.
;; See inline comment further below.
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
;;

;;; Code:

;; Make sure that the following function is patched correctly to prevent
;; serious performance degradation after some time as a result of setting up
;; the timer multiple times.  This must be done every time lsp is upgraded.
(cl-defmethod dap-handle-event ((_event (eql dart.progressStart)) _session params)
  "Handle debugger uris EVENT for SESSION with PARAMS."
  (setq lsp-dart-dap--flutter-progress-reporter
        (make-progress-reporter (concat lsp-dart-dap--debug-prefix (gethash "message" params))))
  ;; BUGFIX: do NOT set up the timer more than once, as over time this leads to
  ;; many dozens of repeated timers running simulateneously and serious
  ;; performance degradation.
  (unless lsp-dart-dap--flutter-progress-reporter-timer
    (setq lsp-dart-dap--flutter-progress-reporter-timer
          (run-with-timer 0.2 0.2 #'lsp-dart-dap--flutter-tick-progress-update))))

(defun init/lsp ()
  "Initialise LSP."
  ;; Enable plists when the required environment var is set to true.
  ;;
  ;; Note that an experiment was conducted to ascertain the possibility of
  ;; improving performance of parsing of LSP messages but the verdict was that
  ;; it significantly worsened performance when editing Dart/Flutter source
  ;; files, and also generally other modes too though to a lesser extent.
  ;;
  ;; Ref: https://emacs-lsp.github.io/lsp-mode/page/performance/
  (let ((use-plists (string= (getenv "LSP_USE_PLISTS") "true")))
    (when use-plists
      (message "Warning: LSP_USE_PLISTS environment var set: performance likely
to degrade under LSP")
      (setq lsp-use-plists use-plists)))

  ;; Refer to initialisation of `gc-cons-threshold' and
  ;; `read-process-output-max' in `internal/settings.el'.
  ;;
  ;; The following as per the documentation at:
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq lsp-log-io nil)

  ;; Don't keep workspace alive once the last buffer is killed.
  (setq lsp-keep-workspace-alive nil)

  ;; Auto-guess the root directory of the buffer
  (setq lsp-auto-guess-root t)

  ;; May make sense to limit the number of signature doc lines?  Not for now,
  ;; though.
  ;; (setq lsp-signature-doc-lines 5)

  ;; Suggested setting found on:
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq lsp-idle-delay 0.5)

  ;; The following section from:
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  ;;
  ;; Highlighting of symbol on point
  (setq lsp-enable-symbol-highlighting t)
  ;; Show documentation for symbol on point
  (setq lsp-ui-doc-show-with-cursor nil)
  ;; Disable lens
  (setq lsp-lens-enable nil)
  ;; Enable headerline showing source file location in workspace
  (setq lsp-headerline-breadcrumb-enable t)
  ;; Enable documentation for symbol on hover
  (setq lsp-eldoc-enable-hover t)
  ;; Signature auto activation
  ;; Can manually request them via `lsp-signature-activate`
  (setq lsp-signature-auto-activate
        '(:on-trigger-char :after-completion :on-server-request))
  (setq lsp-signature-render-documentation t)

  ;; Sideline:
  ;; Disable intrusive code actions and symbol on hover but do show diagnostics
  ;; in sideline
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-symbol nil)
  (setq lsp-ui-sideline-show-diagnostics nil)

  ;; Modeline:
  ;; Disable code actions
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-modeline-workspace-status-enable nil);

  ;; Completion
  (setq lsp-completion-show-detail t)
  (setq lsp-completion-show-kind t))

(defun init/lsp/config ()
  ;; This can't be in the initializing defun above or it'll error out.
  ;; Ref: https://github.com/emacs-lsp/lsp-mode/issues/1532#issuecomment-602384182
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

  ;; It's important that we always fallback to `lsp-format-buffer' when
  ;; prettier isn't being used.
  (unless (and (boundp 'prettier-mode) prettier-mode)
    (add-hook 'before-save-hook 'lsp-format-buffer nil t))) ; local hook

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
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . init/lsp/config))
  :init (init/lsp))

;;; lsp.el ends here
