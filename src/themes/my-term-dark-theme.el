;;; my-term-dark-theme.el --- Personal dark terminal theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026  Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>

;;; Commentary:

;; Dark terminal theme designed to layer on top of `wombat'.  Darkens the
;; background to near-black and tones down highlight faces accordingly.
;; See also `my-gui-dark' for the graphical counterpart.
;;
;; Hex colors are used instead of 256-color names (e.g. "#080808" rather
;; than "color-232") because `custom-theme-set-faces' requires standard
;; color specs.  On truecolor terminals they render exactly; on 256-color
;; terminals Emacs maps them to the nearest palette entry, which is the
;; same color.

;;; Code:

(deftheme my-term-dark
  "Personal dark terminal theme, layers on wombat.")

(custom-theme-set-faces
 'my-term-dark

 ;; Near-black background
 '(default ((t (:background "#080808"))))

 ;; Wombat's highlight is too bright for a near-black background; darken it
 ;; so hl-line and magit-section-highlight (which inherit it) stay subtle.
 '(highlight ((t (:background "#262626" :underline nil))))
 '(hl-line ((t (:inherit highlight :underline nil))))
 '(magit-section-highlight ((t (:inherit highlight)))))

(provide-theme 'my-term-dark)

;;; my-term-dark-theme.el ends here
