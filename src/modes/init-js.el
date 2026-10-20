;;; init-js.el --- Configure JavaScript and JSX modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/js/enable ()
  "Configures modes related to Javascript development."
  (init/common-web-programming-mode))

(use-package js
  :ensure nil
  :mode (("\\.jsx?\\'" . js-ts-mode)
         ("\\.mjs\\'" . js-ts-mode))
  :interpreter (("node\\(?:js\\)?" . js-ts-mode))
  :custom
  (js-indent-level 2)
  :hook ((js-ts-mode js-mode) . init/js/enable))

(provide 'init-js)

;;; init-js.el ends here
