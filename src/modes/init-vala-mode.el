;;; init-vala-mode.el --- Configure Vala mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/vala-mode/enable ()
  "Configure `vala-mode'."

  (init/common-nonweb-programming-mode)
  (c-toggle-auto-hungry-state -1)

  ;; Make sure to disable tabs mode to ensure buffer is formatted with spaces.
  (setq-local indent-tabs-mode nil))

(use-package vala-mode
  :hook ((vala-mode . init/vala-mode/enable)))

(provide 'init-vala-mode)

;;; init-vala-mode.el ends here
