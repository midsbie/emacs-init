;;; my-gui-light-theme.el --- Personal light GUI theme with Tango ANSI colors  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026  Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>

;;; Commentary:

;; Light GUI theme built on the default Emacs theme with Tango ANSI colors
;; and curated programming face colors.  See also `my-gui-dark' for the
;; dark counterpart.

;;; Code:

(deftheme my-gui-light
  "Personal light theme with Tango ANSI colors.")

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

 ;; ANSI colors (Tango palette)
 '(ansi-color-black ((t (:foreground "#2e3436" :background "#2e3436"))))
 '(ansi-color-red ((t (:foreground "#cc0000" :background "#cc0000"))))
 '(ansi-color-green ((t (:foreground "#4e9a06" :background "#4e9a06"))))
 '(ansi-color-yellow ((t (:foreground "#c4a000" :background "#c4a000"))))
 '(ansi-color-blue ((t (:foreground "#3465a4" :background "#3465a4"))))
 '(ansi-color-magenta ((t (:foreground "#75507b" :background "#75507b"))))
 '(ansi-color-cyan ((t (:foreground "#06989a" :background "#06989a"))))
 '(ansi-color-white ((t (:foreground "#d3d7cf" :background "#d3d7cf"))))
 '(ansi-color-bright-black ((t (:foreground "#555753" :background "#555753"))))
 '(ansi-color-bright-red ((t (:foreground "#ef2929" :background "#ef2929"))))
 '(ansi-color-bright-green ((t (:foreground "#8ae234" :background "#8ae234"))))
 '(ansi-color-bright-yellow ((t (:foreground "#fce94f" :background "#fce94f"))))
 '(ansi-color-bright-blue ((t (:foreground "#729fcf" :background "#729fcf"))))
 '(ansi-color-bright-magenta ((t (:foreground "#ad7fa8" :background "#ad7fa8"))))
 '(ansi-color-bright-cyan ((t (:foreground "#34e2e2" :background "#34e2e2"))))
 '(ansi-color-bright-white ((t (:foreground "#eeeeec" :background "#eeeeec")))))

(provide-theme 'my-gui-light)

;;; my-gui-light-theme.el ends here
