;;; my-gui-light-theme.el --- Personal light GUI theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026  Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>

;;; Commentary:

;; Light GUI theme built on the default Emacs theme with curated
;; programming face colors.  Pair with `my-tango-ansi' for ANSI colors.
;; See also `my-gui-dark' for the dark counterpart.

;;; Code:

(deftheme my-gui-light
  "Personal light GUI theme.")

(custom-theme-set-faces
 'my-gui-light

 ;; Base
 '(hl-line ((t (:inherit highlight :background "#ceeace"))))

 ;; Font lock
 '(font-lock-doc-face ((t (:foreground "DarkRed"))))
 '(font-lock-property-use-face ((t (:foreground "SaddleBrown"))))
 '(font-lock-function-call-face ((t (:inherit nil :foreground "Gray20"))))
 '(font-lock-number-face ((t (:inherit font-lock-warning-face :weight normal))))
 '(font-lock-operator-face ((t (:inherit font-lock-builtin-face))))

 ;; LSP
 '(lsp-headerline-breadcrumb-path-face ((t (:inherit font-lock-builtin-face :weight normal))))
)

(provide-theme 'my-gui-light)

;;; my-gui-light-theme.el ends here
