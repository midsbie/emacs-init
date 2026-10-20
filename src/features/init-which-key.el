;;; init-which-key.el --- Configure which-key  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configuration adapted from:
;; https://github.com/justbur/emacs-which-key
;; https://elpa.gnu.org/packages/which-key.html#orgf035424

;;; Code:

(use-package which-key
  :ensure nil
  :diminish
  :custom
  ;; Show only when requested, then refresh quickly.
  (which-key-idle-delay 10000.0)
  (which-key-idle-secondary-delay 0.05)
  (which-key-show-early-on-C-h t)
  (which-key-side-window-max-width 0.5)
  :config
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

(provide 'init-which-key)

;;; init-which-key.el ends here
