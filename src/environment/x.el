;;; x.el --- Initialisation sequence when run within X

;; Copyright (C) 2014-2022 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; URL:
;; Keywords: tools
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; Set default window size
(defun set-frame-size-according-to-resolution ()
  "Set the active frame's size according to the screen's
resolution.  Frame size is set to 190 characters wide if the
display width is larger than 1280, otherwise it is set to 90
characters wide."
  (interactive)
  (when window-system
    ;; use 190 char wide window for largeish displays
    ;; and smaller 90 column windows for smaller displays
    (if (> (x-display-pixel-width) 1280)
        (add-to-list 'default-frame-alist (cons 'width 190))
      (add-to-list 'default-frame-alist (cons 'width 90)))
    (add-to-list 'default-frame-alist (cons 'height 77))))

;; Function: Set current frame width
(defun set-current-frame-width (arg)
  "Set the active frame's width to ARG."
  (interactive "p")
  (set-frame-width (selected-frame) arg))

;; Function: Set current frame height
(defun set-current-frame-height (arg)
  "Set the active frame's height to ARG."
  (interactive "p")
  (set-frame-height (selected-frame) arg))

(when window-system
  ;; This should be improved upon to take into account the dimensions of the
  ;; display and make splitting behave accordingly.
  (setq split-width-threshold nil
        split-height-threshold nil)

  (setq speedbar-use-images nil)          ; do not use icons in `speedbar-mode'

  ;; Uncomment if desired that the window should be maximised
  ; (toggle-frame-maximized)

  ;; Highlighter
  ;;
  ;; ?????? Disabled as it was seriously degrading performance
  ;;
  ;; 260621 Circumstances which led to disabling of highlighter are unclear.
  ;;        Tentatively reactivating since many package upgrades have taken
  ;;        place since.
  (global-hl-line-mode 1)

  ;; Enable clipboard functionality
  (setq select-enable-clipboard     t)

  ;; Dynamically setting active project name.
  ;; From: https://emacs.stackexchange.com/a/35443
  (setq frame-title-format '((:eval
                              (let ((project-name (projectile-project-name)))
                                (unless (string= "-" project-name)
                                  (format "%s :: " project-name))))
                             "%b"))

  ;; The following themes in order of reverse preference:
  ;;
  ;; - Difficult to read lighter faces.
  ;; - Integration with `vertico' could be improved.
  ;; (load-theme 'solarized-selenized-light t)
  ;;
  ;; - An improvement over solarized light but still feels a little washed out,
  ;;   making it difficult to distinguish some faces whose foreground color are
  ;;   approximate the background color.
  ;; + GREAT harmonized face colors for programming.
  ;; (load-theme 'solarized-zenburn t)
  ;;
  ;; + THE BEST theme in terms of readability.
  ;; - BAD choise of face colors for programming; e.g. purple for keywords
  ;; (load-theme 'tango t)
  ;;
  ;; * Evaluating themes below:
  ;; (load-theme 'gruvbox-dark-soft t)
  ;; (load-theme 'modus-operandi t)

  ;; Disabled
  ;;
  ;;   (setq solarized-distinct-fringe-background  t
  ;;         solarized-distinct-doc-face           t
  ;;         solarized-high-contrast-mode-line     t)
  ;;   (load-theme 'solarized-zenburn t)

  (custom-set-faces
   ;; Default face; note that any subsequent face customization calls on
   ;; 'default are likely to not result in the intended result; consider using
   ;; `set-face-attribute' instead.
   '(default ((t (:inherit nil :stipple nil
                           :inverse-video nil :box nil
                           :strike-through nil :overline nil :underline nil
                           :slant normal :weight normal :height 85 :width normal
                           :family "Hack"))))
   '(lsp-headerline-breadcrumb-path-face ((t (:inherit font-lock-builtin-face :weight normal))))
   ;; Not set by default, for some reason.
   '(font-lock-number-face ((t (:inherit font-lock-warning-face :weight normal))))
   '(font-lock-operator-face ((t (:inherit font-lock-builtin-face)))))

  (add-hook 'emacs-startup-hook #'init/customize-default-theme)

  ;; Eval the following statement when debugging or trying out new faces:
  ;; (custom-set-faces '(region ((t (:background "#4f5f60" :foreground nil)))))

  ;; Let's now perform last initialization steps.
  ;;
  ;; Properties can be accessed with the following form:
  ;;
  ;;   (cdr (assoc 'width default-frame-alist))
  (add-to-list 'default-frame-alist (cons 'width 120))
  (add-to-list 'default-frame-alist (cons 'height 70))

  (let* ((width (cdr (assoc 'width default-frame-alist)))
         (height (cdr (assoc 'height default-frame-alist))))
    (and width (set-current-frame-width width))
    (and height (set-current-frame-width height)))

  ;; Re-balance windows now that the frame's size has been calculated
  (balance-windows)

  ;; For some reason, version >25.x seems to activate the horizontal scrollbars.
  ;; We forcefully disable them here.
  (when (boundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  )

(defun init/customize-default-theme ()
  "Customize default theme if no custom theme loaded."
  (unless custom-enabled-themes
    ;; Tango color scheme for ANSI faces
    (set-face-attribute 'ansi-color-black nil :foreground "#2e3436" :background "#2e3436")
    (set-face-attribute 'ansi-color-red nil :foreground "#cc0000" :background "#cc0000")
    (set-face-attribute 'ansi-color-green nil :foreground "#4e9a06" :background "#4e9a06")
    (set-face-attribute 'ansi-color-yellow nil :foreground "#c4a000" :background "#c4a000")
    (set-face-attribute 'ansi-color-blue nil :foreground "#3465a4" :background "#3465a4")
    (set-face-attribute 'ansi-color-magenta nil :foreground "#75507b" :background "#75507b")
    (set-face-attribute 'ansi-color-cyan nil :foreground "#06989a" :background "#06989a")
    (set-face-attribute 'ansi-color-white nil :foreground "#d3d7cf" :background "#d3d7cf")
    (set-face-attribute 'ansi-color-bright-black nil :foreground "#555753" :background "#555753")
    (set-face-attribute 'ansi-color-bright-red nil :foreground "#ef2929" :background "#ef2929")
    (set-face-attribute 'ansi-color-bright-green nil :foreground "#8ae234" :background "#8ae234")
    (set-face-attribute 'ansi-color-bright-yellow nil :foreground "#fce94f" :background "#fce94f")
    (set-face-attribute 'ansi-color-bright-blue nil :foreground "#729fcf" :background "#729fcf")
    (set-face-attribute 'ansi-color-bright-magenta nil :foreground "#ad7fa8" :background "#ad7fa8")
    (set-face-attribute 'ansi-color-bright-cyan nil :foreground "#34e2e2" :background "#34e2e2")

    (cond
     (init/defaults/light-theme
      (set-face-attribute 'hl-line nil :inherit 'highlight :background "#ceeace")

      (set-face-attribute 'font-lock-doc-face nil :foreground "DarkRed")
      (set-face-attribute 'font-lock-property-use-face nil :foreground "SaddleBrown")
      (set-face-attribute 'font-lock-function-call-face nil
                          :inherit nil :foreground "Gray20"))

     ((not init/defaults/light-theme)
      ;; Calling `custom-set-faces' a second time on 'default will not work
      ;; because the settings would be merged and, it seems, the first call's
      ;; settings would win resulting in a nil foreground and background.  Must
      ;; therefore use `set-face-attribute'.
      (set-face-attribute 'default nil
                          :foreground "#ffffff"
                          :background "#000000")

      (set-face-attribute 'widget-field nil :foreground "black")

      ;; Inherited by `swiper-match-face-1'
      (set-face-attribute 'lazy-highlight nil :foreground "white" :background "blue4")
      ;; Inherited by `swiper-match-face-2'
      (set-face-attribute 'isearch nil :foreground "black")

      (set-face-attribute 'isearch-group-1 nil :foreground "dark blue")
      (set-face-attribute 'isearch-group-2 nil :foreground "dark blue")

      ;; Doesn't seem to be defined:
      ;; (set-face-attribute 'popup-isearch-match nil :foreground "dark blue")

      (set-face-attribute 'swiper-match-face-1 nil :foreground "white")
      (set-face-attribute 'swiper-match-face-2 nil :foreground "black")
      (set-face-attribute 'swiper-match-face-3 nil :foreground "white")
      (set-face-attribute 'swiper-match-face-4 nil :foreground "white")

      (use-package magit
        :config
        (set-face-attribute 'magit-diff-added nil :foreground "white smoke")
        (set-face-attribute 'magit-diff-added-highlight nil :foreground "white smoke")
        (set-face-attribute 'magit-diff-removed nil :foreground "white smoke")
        (set-face-attribute 'magit-diff-removed-highlight nil :foreground "white smoke"))

      (use-package company
        :config
        (set-face-attribute 'company-tooltip nil :background "gray10")
        (set-face-attribute 'company-tooltip-selection nil :inherit 'highlight)
        (set-face-attribute 'company-tooltip-search nil :inherit 'consult-highlight-match))

      ;; Doesn't seem to be defined:
      ;; (set-face-attribute 'pulse-highlight-start-face nil :foreground "dark blue")
      (set-face-attribute 'speedbar-highlight-face nil :foreground "dark blue")

      (set-face-attribute 'font-lock-doc-face nil :foreground "dark salmon")
      (set-face-attribute 'font-lock-property-use-face nil :foreground "cornsilk")
      (set-face-attribute 'font-lock-function-call-face nil
                          :inherit nil :foreground "LightBlue")

      (set-face-attribute 'mode-line-inactive nil
                          :inherit 'mode-line-active
                          :background "grey30")

      ;; Another possibility here is "midnight blue", or considering a darker
      ;; still variation of the dark green #004225, possibly with a blue-ish ink.
      (set-face-attribute 'highlight nil :background "#004225")
      (set-face-attribute 'hl-line nil :inherit 'highlight :background "#000f08")
      ))))

;; These customizations kept for posteriority in case a decision is made to go
;; back to a solarized-based theme.
(use-package solarized-theme
  :config
  (cond ((member 'solarized-zenburn custom-enabled-themes)
         (require 'solarized-palettes)
         (custom-theme-set-faces
          'solarized-zenburn
          `(font-lock-preprocessor-face
            ((t (:weight bold :foreground ,(cdr (assoc 'red-l solarized-zenburn-color-palette-alist))))))
          `(font-lock-string-face
            ((t (:foreground ,(cdr (assoc 'orange solarized-zenburn-color-palette-alist))))))
          `(font-lock-number-face
            ((t (:foreground ,(cdr (assoc 'orange-2fg solarized-zenburn-color-palette-alist))))))
          `(font-lock-constant-face
            ((t (:foreground ,(cdr (assoc 'orange-2fg solarized-zenburn-color-palette-alist))))))
          `(font-lock-function-name-face
            ((t (:foreground ,(cdr (assoc 'magenta solarized-zenburn-color-palette-alist))))))
          `(my-function-call-face
            ((t ( :foreground ,(cdr (assoc 'magenta-2fg solarized-zenburn-color-palette-alist))))))
          `(font-lock-function-call-face
            ((t (:override t :inherit my-function-call-face :foreground ,(cdr (assoc 'magenta-2fg solarized-zenburn-color-palette-alist))))))
          `(font-lock-variable-use-face
            ((t (:foreground ,(cdr (assoc 'cyan-2fg solarized-zenburn-color-palette-alist))))))
          `(font-lock-property-use-face
            ((t (:foreground ,(cdr (assoc 'base0 solarized-zenburn-color-palette-alist))))))
          `(company-tooltip-selection ((t (:background "#606060" :weight bold))))
          `(region ((t (:foreground nil :background "#4f5f60")))))))
  )

;;; x.el ends here
