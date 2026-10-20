;;; init-recursion-indicator.el --- Configure recursion-indicator  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package recursion-indicator
  :demand t
  :diminish
  :config
  (recursion-indicator-mode t))

(provide 'init-recursion-indicator)

;;; init-recursion-indicator.el ends here
