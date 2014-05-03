;; init-x.el --- Initialisation sequence when run within X
;;
;; Copyright (C) 2014 Miguel Guedes
;;
;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; URL: 
;; Keywords: init
;; Version: 1.0
;;
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
;;
;; Comments:
;; 
;;

;; Set default window size
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
      (progn
        ;; use 180 char wide window for largeish displays
        ;; and smaller 90 column windows for smaller displays
        (if (> (x-display-pixel-width) 1280)
            (add-to-list 'default-frame-alist (cons 'width 190))
          (add-to-list 'default-frame-alist (cons 'width 90)))
        (add-to-list 'default-frame-alist 
                     (cons 'height 77)))))

;; Function: Set current frame width
(defun set-current-frame-width (arg)
  (interactive "p")
  (set-frame-width (selected-frame) arg))

;; Function: Set current frame height
(defun set-current-frame-height (arg)
  (interactive "p")
  (set-frame-height (selected-frame) arg))


(when window-system
  (setq frame-title-format "%b - emacs")
  (tool-bar-mode -1)                      ; disable toolbar

  ;; custom faces
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:inherit nil :stipple nil :background "#080808" :foreground
                           "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil
                           :underline nil :slant normal :weight normal :height 84 :width normal :foundry
                           "unknown" :family "Droid Sans Mono"))))
   '(rst-level-1-face ((t (:weight bold))) t)
   '(rst-level-2-face ((t (:weight bold))) t)
   '(rst-level-3-face ((t (:weight extra-bold))) t))
  
  ;; Let's now perform last initialization steps
  (add-to-list 'default-frame-alist (cons 'width 185))
  (add-to-list 'default-frame-alist (cons 'height 73))
  
  (set-current-frame-width 185)
  (set-current-frame-height 73)
  
  ;; Highlighter
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "#383830")

  ;; Enable clipboard functionality
  (setq x-select-enable-clipboard t
	interprogram-paste-function 'x-cut-buffer-or-selection-value)

  (split-window-horizontally)             ; two windows at startup
  )