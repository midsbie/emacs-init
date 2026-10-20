;;; init-html-ts-mode.el --- Configure HTML modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `web-mode' is currently used for editing HTML files.

;;; Code:

;; `.html' files open in `web-mode' (modes/init-web-mode.el).
(use-package html-ts-mode
  :ensure nil
  :hook ((html-mode html-ts-mode) . init/common-web-programming-mode))

(provide 'init-html-ts-mode)

;;; init-html-ts-mode.el ends here
