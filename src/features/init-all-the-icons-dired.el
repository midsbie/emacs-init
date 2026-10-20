;;; init-all-the-icons-dired.el --- Configure all-the-icons-dired  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package all-the-icons-dired
  :diminish
  :hook ((dired-mode . all-the-icons-dired-mode)))

(provide 'init-all-the-icons-dired)

;;; init-all-the-icons-dired.el ends here
