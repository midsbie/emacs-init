;;; init-xclip.el --- Configure xclip  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; System clipboard integration for terminal Emacs.

;;; Code:

(use-package xclip
  :if (not (display-graphic-p))
  :config
  ;; Headless hosts may have no clipboard tool.
  (when (executable-find xclip-program)
    (xclip-mode 1)))

(provide 'init-xclip)

;;; init-xclip.el ends here
