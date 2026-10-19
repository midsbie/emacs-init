;;; init-recursion-indicator.el --- Configure recursion-indicator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configuration for the recursion-indicator library.

;;; Code:

(use-package recursion-indicator
  :demand t
  :diminish
  :config
  (recursion-indicator-mode t))

(provide 'init-recursion-indicator)

;;; init-recursion-indicator.el ends here
