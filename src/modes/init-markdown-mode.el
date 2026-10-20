;;; init-markdown-mode.el --- Configure Markdown mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/markdown-mode/enable ()
  "Configure `markdown-mode' buffers."
  (visual-line-mode 1))

(use-package markdown-mode
  :defer t
  :hook (markdown-mode . init/markdown-mode/enable)
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-enable-math t)
  (markdown-enable-wiki-links t))

(provide 'init-markdown-mode)

;;; init-markdown-mode.el ends here
