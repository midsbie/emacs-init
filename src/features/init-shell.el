;;; init-shell.el --- Configure shell  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/shell/filter-output (output)
  "Remove Node.js terminal-control sequences from comint OUTPUT."
  (replace-regexp-in-string "\e\\[\\??[0-9]+[hlAGK]\n?" "" output))

(use-package shell
  :ensure nil
  :defer t
  :config
  (add-hook 'comint-preoutput-filter-functions #'init/shell/filter-output))

(provide 'init-shell)

;;; init-shell.el ends here
