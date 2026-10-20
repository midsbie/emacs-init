;;; init-indent-bars.el --- Configure indent-bars  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: internal, tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package indent-bars
  :hook ((python-mode python-ts-mode yaml-mode yaml-ts-mode) . indent-bars-mode))

(provide 'init-indent-bars)

;;; init-indent-bars.el ends here
