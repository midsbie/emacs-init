(when (eq window-system 'x)  
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
  
  (set-frame-width-interactive 185)
  (set-frame-height-interactive 73)
  
  ;; Highlighter
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "#383830")

  ;; Enable clipboard functionality
  (setq x-select-enable-clipboard t
	interprogram-paste-function 'x-cut-buffer-or-selection-value)

  (split-window-horizontally)             ; two windows at startup
  )
