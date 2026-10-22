;;; init-shell.el --- Configure shell  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/shell/filter-output (output)
  "Remove Node.js terminal-control sequences from comint OUTPUT."
  (replace-regexp-in-string "\e\\[\\??[0-9]+[hlAGK]" "" output))

(defun init/shell/enable ()
  "Filter terminal-control sequences from this shell buffer's output."
  (add-hook 'comint-preoutput-filter-functions #'init/shell/filter-output nil t))

(use-package shell
  :ensure nil
  :defer t
  :hook (shell-mode . init/shell/enable))

(provide 'init-shell)

;;; init-shell.el ends here
