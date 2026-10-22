;;; my-gui-dark-theme.el --- Personal dark GUI theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Dark GUI theme built on the default Emacs theme with curated programming
;; face colors for readability on a near-black background.  Pair with
;; `my-tango-ansi' for ANSI colors.  See also `my-term-dark' for the
;; terminal counterpart.

;;; Code:

(require 'color)

(deftheme my-gui-dark
  "Personal dark GUI theme.")

(let ((ml-active-bg "#1a2d4a"))
  (custom-theme-set-faces
   'my-gui-dark

   ;; Base
   '(default ((t (:foreground "#ffffff" :background "#080808"))))
   '(highlight ((t (:background "#004225"))))
   '(hl-line ((t (:inherit highlight :background "#000f08"))))

   ;; Mode line (original background: "grey22")
   `(mode-line-active ((t (:foreground "#d3d7cf" :background ,ml-active-bg
                            :box (:line-width 1 :color ,(color-lighten-name ml-active-bg 15))))))
   '(mode-line-inactive ((t (:inherit mode-line-active
                              :foreground "grey50" :background "grey10"
                              :box (:line-width 1 :color "grey14")))))

   ;; Font lock
   '(font-lock-doc-face ((t (:foreground "dark salmon"))))
   '(font-lock-property-use-face ((t (:foreground "cornsilk"))))
   '(font-lock-function-call-face ((t (:inherit nil :foreground "LightBlue"))))
   '(font-lock-number-face ((t (:inherit font-lock-warning-face :weight normal))))))

(provide-theme 'my-gui-dark)

;;; my-gui-dark-theme.el ends here
