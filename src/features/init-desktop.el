;;; init-desktop.el --- Configure desktop  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package desktop
  :ensure nil
  :defer t
  :hook (;; Set our preferred window arrangement after successfully loading a
         ;; desktop configuration.
         (desktop-after-read . my/rearrange-desktop))
  :config
  ;; Make it so `desktop-change-dir' and `desktop-save' prompt for the current
  ;; working directory by default.
  (add-to-list 'desktop-path ".")
  )

(provide 'init-desktop)

;;; init-desktop.el ends here
