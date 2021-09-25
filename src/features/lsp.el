;;; lsp.el --- Configures the `lsp' package

;; Copyright (C) 2021  Miguel Guedes

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

(defun init/mode/lsp ()
  "Initialise LSP mode."
  ;; Refer to initialisation of `gc-cons-threshold' and `read-process-output-max'
  ;; in `internal/settings.el'.
  ;;
  ;; The following as per the documentation at:
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq lsp-log-io nil)

  ;; Don't keep workspace alive once the last buffer is killed.
  (setq lsp-keep-workspace-alive nil)

  ;; May make sense to limit the number of signature doc lines?  Not for now,
  ;; though.
  ;; (setq lsp-signature-doc-lines 5)
  (setq lsp-idle-delay 0.5)

  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package lsp-mode
  :ensure t
  :init
  (init/mode/lsp))

;;; lsp.el ends here
