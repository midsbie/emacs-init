;;; init-all-the-icons-dired.el --- Configure all-the-icons-dired -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configuration for the all-the-icons-dired library.

;;; Code:

(use-package all-the-icons-dired
  :diminish
  :hook ((dired-mode . all-the-icons-dired-mode)))

(provide 'init-all-the-icons-dired)

;;; init-all-the-icons-dired.el ends here
