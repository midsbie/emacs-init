;;; init-dabbrev.el --- Configure dabbrev  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package dabbrev
  :ensure nil
  :custom
  ;; Make it so matching and expansion respects case.  May need to be set for
  ;; prog modes only.
  (dabbrev-case-fold-search nil)
  (dabbrev-case-replace nil))

(provide 'init-dabbrev)

;;; init-dabbrev.el ends here
