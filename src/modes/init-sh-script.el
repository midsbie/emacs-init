;;; init-sh-script.el --- Configure shell script modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/sh-script/enable ()
  "Initialise modes related to shell scripting development."
  (init/common-nonweb-programming-mode))

(use-package sh-script
  :ensure nil
  :hook ((sh-mode bash-ts-mode) . init/sh-script/enable)
  :custom
  (sh-basic-offset 2)
  :init
  ;; Bash's tree-sitter mode redirects scripts for other shells to sh-mode.
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode)))

(provide 'init-sh-script)

;;; init-sh-script.el ends here
