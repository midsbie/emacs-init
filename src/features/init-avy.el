;;; init-avy.el --- Configure avy  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/avy/config ()
  "Configure `avy' package."
  (avy-setup-default)
)

(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("C-c C-j" . avy-resume))
  :config (init/avy/config))

(provide 'init-avy)

;;; init-avy.el ends here
