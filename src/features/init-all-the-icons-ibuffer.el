;;; init-all-the-icons-ibuffer.el --- Configure all-the-icons-ibuffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package all-the-icons-ibuffer
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(provide 'init-all-the-icons-ibuffer)

;;; init-all-the-icons-ibuffer.el ends here
