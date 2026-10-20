;;; init-yaml-ts-mode.el --- Configure YAML modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: internal, tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/yaml-ts-mode/enable ()
  (init/common-programming-mode))

(use-package yaml-ts-mode
  :ensure nil
  :hook ((yaml-mode yaml-ts-mode) . init/yaml-ts-mode/enable)
  :mode ("\\.yml\\'" "\\.yaml\\'" "\\.eslintrc\\'"))

(provide 'init-yaml-ts-mode)

;;; init-yaml-ts-mode.el ends here
