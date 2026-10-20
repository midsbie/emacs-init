;;; init-css-mode.el --- Configure CSS modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/css-mode/enable ()
  "Configure `css-mode'."
  (init/common-nonweb-programming-mode))

(use-package css-mode
  :ensure nil
  :bind (:map css-mode-map
         ("}" . my/insert-and-indent)
         (";" . my/insert-and-indent)
         :map css-ts-mode-map
         ("}" . my/insert-and-indent)
         (";" . my/insert-and-indent))
  :mode (("\\.css\\'" . css-ts-mode))
  :custom
  (css-indent-offset 2)
  ;; SCSS inherits css-mode-hook; configuring it again would start Eglot twice.
  :hook ((css-mode css-ts-mode) . init/css-mode/enable))

(provide 'init-css-mode)

;;; init-css-mode.el ends here
