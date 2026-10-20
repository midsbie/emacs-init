;;; init-goggles.el --- Configure goggles  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package goggles
  :diminish
  :hook ((prog-mode text-mode) . goggles-mode)
  :custom
  (goggles-pulse t))

(provide 'init-goggles)

;;; init-goggles.el ends here
