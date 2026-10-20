;;; init-gcmh.el --- Configure gcmh  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package gcmh
  :diminish
  :custom
  (gcmh-idle-delay 5)
  (gcmh-high-cons-threshold (* 16 1024 1024)) ; 16 MiB
  :config
  (gcmh-mode 1))

(provide 'init-gcmh)

;;; init-gcmh.el ends here
