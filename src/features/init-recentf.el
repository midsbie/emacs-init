;;; init-recentf.el --- Configure recentf  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package recentf
  :ensure nil
  :demand
  ;; The following replaces default binding to `ido-find-file-read-only'
  ;; :bind ("C-x C-r" . recentf-open-files)

  :custom
  (recentf-auto-cleanup    'never)
  ;; Caps `recentf-list', which `consult-recent-file' (C-x C-r) reads.
  (recentf-max-saved-items 100)

  :config
  (recentf-mode 1)
  )

(provide 'init-recentf)

;;; init-recentf.el ends here
