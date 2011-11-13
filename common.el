(add-to-list 'load-path "/usr/share/emacs/common-lisp/php-mode-1.5.0/")
(add-to-list 'load-path "/usr/share/emacs/common-lisp/css-mode/")
(add-to-list 'load-path "/usr/share/emacs/common-lisp/linum/")
(add-to-list 'load-path "/usr/share/emacs/common-lisp/color-theme-6.6.0/")
(add-to-list 'load-path "/usr/share/emacs/common-lisp/python-mode/")
(add-to-list 'load-path "/usr/share/emacs/common-lisp/cc-mode-5.31.3/")


;; zen-coding mode
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes
(global-set-key (kbd "M-RET") 'zencoding-expand-line) ;; Bind M-RET

;; load espresso-mode
(autoload #'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

;;(require 'find-recursive)

;; load cc-mode
(autoload 'awk-mode "cc-mode" nil t)
(add-hook 'c-mode-common-hook
          (lambda () (c-toggle-auto-hungry-state 1)
            (c-toggle-auto-state -1)))


(load "linum")

;; php-mode
(load "php-mode")
(add-to-list 'auto-mode-alist
             '("\\.php[34]?\\'\\|\\.phtml\\'" . php-mode))

;; css-mode
(load "css-mode")
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

;; python-mode  
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-hook 'python-mode-hook 'elide-head)
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(autoload 'python-mode "python-mode" "Python editing mode." t)
(autoload 'py-shell "python-mode" "Python shell." t)

;; we hate tabs!
(setq c-default-style "linux"
      c-basic-offset 2)
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

(add-hook 'java-mode-hook
          '(lambda ()
             (make-local-variable 'write-contents-hooks)
             (add-hook 'write-contents-hooks 'java-mode-untabify)))

;;Add u-color-cycle to zone-programs
(eval-after-load 'zone
  '(progn
     (load-library "u-color-cycle")
     (setq zone-programs (vconcat zone-programs [ u-color-cycle-window ]))))


;; Highlighter
(global-hl-line-mode 1)
(set-face-background 'hl-line "#330")

;; Text mode is happier than Fundamental mode
(setq default-major-mode 'text-mode)

;; Enable clipboard functionality
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; ---- Aliases ----
(defalias 'hscroll-mode 'toggle-truncate-lines)
;;Alias query functions so all the replace functions start with "replace"!
(defalias 'replace-query 'query-replace)
(defalias 'replace-query-regexp 'query-replace-regexp)
(defalias 'replace-query-regexp-eval 'query-replace-regexp-eval)

;;(require 'cua-base)
;;(recentf-mode t)

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-chocolate-rain)))

;; Keyboard shortcuts
(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "C-]") 'scroll-down)
(global-set-key (kbd "C-#") 'scroll-up)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; awesome key bindings for moving around windows
(global-set-key [M-left] 'windmove-left)          ; move to left window
(global-set-key [M-right] 'windmove-right)        ; move to right window
(global-set-key [M-up] 'windmove-up)              ; move to upper window
(global-set-key [M-down] 'windmove-down)          ; move to downer window

;; (global-set-key (kbd "C-S-w") 'longlines-mode)
(global-set-key (kbd "C-S-w") 'toggle-truncate-lines)
(global-set-key (kbd "M-r") 'revert-buffer)

(global-set-key (kbd "C-c C-t") 'c-toggle-hungry-state)


;; Emacs - different bindings & custom settings
;; the following binding is disabled so as to force myself to use C-j
;; (define-key global-map (kbd "RET") 'newline-and-indent)          DISABLED so as to force myself to use C-j

(keyboard-translate ?\C-h ?\C-?)                  ; assign backspace's abilities to C-h
(global-set-key [(meta h)] 'backward-kill-word)   ; this new key binding replaces mark-paragraph
(global-set-key [(hyper h)] 'help-command)        ; doesn't seem to be working

(setq inhibit-splash-screen t)          ; Disable splash screen
(setq initial-scratch-message nil)      ; Disable startup message
(delete-selection-mode t)               ; Enable C-D to delete selected text
(transient-mark-mode t)                 ; Enable typing to replace selected text

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(comment-multi-line t)
 '(comment-style (quote extra-line))
 '(css-electric-brace-behavior nil)
 '(css-indent-offset 2)
 '(espresso-enabled-frameworks (quote (javascript prototype)))
 '(espresso-expr-indent-offset 2)
 '(espresso-indent-level 2)
 '(global-linum-mode t)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(standard-indent 2)
 '(tab-stop-list (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90 92 94 96 98 100)))
 '(tab-width 2)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "#1a0f0b" :foreground "#c3be98" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "Liberation Mono"))))
 '(Info-title-1-face ((t (:weight bold))) t)
 '(font-lock-comment-face ((t (:foreground "#999d63"))))
 '(linum ((t (:foreground "#696969")))))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'ido)
(ido-mode t)

(setq frame-title-format "%b - emacs")
(put 'upcase-region 'disabled nil)

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

;; Function: Set frame width
(defun set-frame-width-interactive (arg)
   (interactive "p")
   (set-frame-width (selected-frame) arg))

;; Function: Set frame height
(defun set-frame-height-interactive (arg)
   (interactive "p")
   (set-frame-height (selected-frame) arg))
