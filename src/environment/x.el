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

  (setq solarized-distinct-fringe-background  t
        solarized-distinct-doc-face           t
        solarized-high-contrast-mode-line     t)

  (load-theme 'solarized-zenburn t)

  ;; Theme customizations
  (custom-theme-set-faces
   'solarized-zenburn
   '(font-lock-string-face ((t (:foreground "#CC9393"))))
   '(company-tooltip-selection ((t (:background "#606060" :weight bold))))
   '(region ((t (:background "#4f5f60" :foreground nil)))))

  ;; Eval the following statement when debugging or trying out new faces:
  ;; (custom-set-faces '(region ((t (:background "#4f5f60" :foreground nil)))))

  ;; Default face
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil
                           :inverse-video nil :box nil
                           :strike-through nil :overline nil :underline nil
                           :slant normal :weight normal :height 85 :width normal
                           :family "Hack")))))

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

  ;; Completely disable mouse on the 'nyx' laptop.
  (cond ((string= (system-name) "nyx")
         (global-disable-mouse-mode 1)))
  )

;;; x.el ends here
