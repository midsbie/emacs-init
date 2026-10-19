;;; init-all-the-icons-ibuffer.el --- Configure all-the-icons-ibuffer -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configuration for the all-the-icons-ibuffer library.

;;; Code:

(use-package all-the-icons-ibuffer
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(provide 'init-all-the-icons-ibuffer)

;;; init-all-the-icons-ibuffer.el ends here
