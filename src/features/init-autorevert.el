;;; init-autorevert.el --- Configure autorevert  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :config
  ;; Revert buffer if file associated with it changes outside of emacs.
  (global-auto-revert-mode 1))

(provide 'init-autorevert)

;;; init-autorevert.el ends here
