;;; my-tango-ansi-theme.el --- Tango ANSI color palette  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026  Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>

;;; Commentary:

;; Sets the 16 ANSI color faces to the Tango palette.  Designed to be
;; loaded before a personal GUI theme (e.g. `my-gui-dark', `my-gui-light')
;; so the palette is shared rather than duplicated.

;;; Code:

(deftheme my-tango-ansi
  "Tango color palette for the 16 ANSI faces.")

(custom-theme-set-faces
 'my-tango-ansi

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

(provide-theme 'my-tango-ansi)

;;; my-tango-ansi-theme.el ends here
