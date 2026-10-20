;;; init-meson-mode.el --- Configure Meson mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/meson-mode/enable ()
  (init/common-nonweb-programming-mode))

(use-package meson-mode
  :hook ((meson-mode . init/meson-mode/enable)))

(provide 'init-meson-mode)

;;; init-meson-mode.el ends here
