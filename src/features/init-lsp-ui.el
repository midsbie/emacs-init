;;; init-lsp-ui.el --- Configure lsp-ui -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configuration for the lsp-ui library.

;;; Code:

(use-package lsp-ui
  :defer t
  :custom
  (lsp-ui-doc-show-with-cursor t)
  ;; The floating documentation frame obscures source code.
  (lsp-ui-doc-enable nil)
  (lsp-ui-imenu-enable nil)
  ;; Keep diagnostics in the sideline without hover or code-action popups.
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-symbol nil)
  (lsp-ui-sideline-show-diagnostics t))

(provide 'init-lsp-ui)

;;; init-lsp-ui.el ends here
