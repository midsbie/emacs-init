;;; x.el --- Initialisation sequence when run within X

;; Copyright (C) 2014-2018 Miguel Guedes

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
  (if window-system
      (progn
        ;; use 190 char wide window for largeish displays
        ;; and smaller 90 column windows for smaller displays
        (if (> (x-display-pixel-width) 1280)
            (add-to-list 'default-frame-alist (cons 'width 190))
          (add-to-list 'default-frame-alist (cons 'width 90)))
        (add-to-list 'default-frame-alist
                     (cons 'height 77)))))

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
  (setq frame-title-format "%b - emacs")
  (tool-bar-mode -1)                      ; disable toolbar

  ;; Set custom faces
  ; The following call sets the default color of window (buffer) backgrounds,
  ; as well as some other aspects, such as the default foreground color and
  ; font.  Note that a theme is NOT in use and we currently rely entirely on
  ; Emacs' base configuration.
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :background "#080808"
                  :foreground "#dcdccc" :inverse-video nil :box nil
                  :strike-through nil :overline nil :underline nil
                  :slant normal :weight normal :height 84 :width normal
                  :foundry "unknown" :family "DejaVu Sans Mono"))))
   '(rst-level-1-face ((t (:weight bold))) t)
   '(rst-level-2-face ((t (:weight bold))) t)
   '(rst-level-3-face ((t (:weight extra-bold))) t))

  ;; Let's now perform last initialization steps
  (add-to-list 'default-frame-alist (cons 'width 185))
  (add-to-list 'default-frame-alist (cons 'height 73))

  (set-current-frame-width 185)
  (set-current-frame-height 73)

  ;; Highlighter
  ; UPDATE: disabled as it was seriously degrading performance
  ; (global-hl-line-mode 1)
  ; (set-face-background 'hl-line "#383830")

  ;; Enable clipboard functionality
  (setq x-select-enable-clipboard   t
        interprogram-paste-function 'x-cut-buffer-or-selection-value)

  ;; `when'
  (split-window-horizontally)             ; two windows at startup
  (setq speedbar-use-images nil)          ; do not use icons in `speedbar-mode'

  (tooltip-mode -1)                       ; disable tooltips

  ;; For some reason, version >25.x seems to activate the horizontal scrollbars.
  ;; We forcefully disable them here.
  (when (boundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))

  ;; Completely disable mouse on the 'nyx' laptop.
  (cond ((string= (system-name) "nyx")
         (global-disable-mouse-mode 1)))
  )

;;; x.el ends here
