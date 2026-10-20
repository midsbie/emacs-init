;;; init-json-ts-mode.el --- Configure JSON modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package json-ts-mode
  :ensure nil
  :mode ("\\.json\\'")
  :hook ((json-mode json-ts-mode) . init/common-nonweb-programming-mode))

(provide 'init-json-ts-mode)

;;; init-json-ts-mode.el ends here
