;;; init-eldoc.el --- Configure eldoc  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/eldoc/config ()
  (set-face-attribute 'eldoc-highlight-function-argument nil
                      :underline t ; :foreground "green"
                      :weight 'bold))

(use-package eldoc
  :ensure nil
  :diminish
  :config (init/eldoc/config))

(provide 'init-eldoc)

;;; init-eldoc.el ends here
