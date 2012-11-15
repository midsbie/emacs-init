(when (equal window-system "x")
  (menu-bar-mode -1)                      ; disable menu bar
  (tool-bar-mode -1)                      ; disable toolbar
  ;; (custom-set-faces
  '(default ((t (:inherit nil :stipple nil :background "#080808" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 88 :width normal :foundry "unknown" :family "Liberation Mono"))))
;; Let's now perform last initialization steps
  (add-to-list 'default-frame-alist (cons 'width 185))
  (add-to-list 'default-frame-alist (cons 'height 73))
  (set-frame-width-interactive 185)
  (set-frame-height-interactive 73)
  )

