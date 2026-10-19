;;; init-highlight-parentheses.el --- Configure highlight-parentheses -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configuration for the highlight-parentheses library.

;;; Code:

(use-package highlight-parentheses
  :diminish
  :config (global-highlight-parentheses-mode))

(provide 'init-highlight-parentheses)

;;; init-highlight-parentheses.el ends here
