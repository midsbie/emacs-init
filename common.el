(add-to-list 'load-path "/usr/share/emacs/common-lisp/color-themes/")

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
 '(default ((t (:inherit nil :stipple nil :background "#080808" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 91 :width normal :foundry "unknown" :family "Liberation Mono")))))

;; load cc-mode
(autoload 'awk-mode "cc-mode" nil t)
(add-hook 'c-mode-common-hook
          (lambda () (c-toggle-auto-hungry-state 1)
            (c-toggle-auto-state -1)))

;; nxhtml-mode
(load "nxhtml/autostart.el")
;(setq mumamo-background-colors nil)     ;; disable background color changes
(css-color-global-mode)

(defun web-mode()
  (interactive)
  (nxhtml-mumamo-mode))

(add-to-list 'auto-mode-alist '("\\.inc$" .nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . nxhtml-mumamo-mode))

;; FIX for mumamo's annoying warning messages ;;;;;;;;;;;;;;;;
; Mumamo is making emacs 23.3 freak out:
(when (and (equal emacs-major-version 23)
           (equal emacs-minor-version 3))
  (eval-after-load "bytecomp"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-beginning-of-syntax-function))
  ;; tramp-compat.el clobbers this variable!
  (eval-after-load "tramp-compat"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-beginning-of-syntax-function)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; we hate tabs!
(setq c-default-style "linux"
      c-basic-offset 2)
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

(add-hook 'java-mode-hook
          '(lambda ()
             (make-local-variable 'write-contents-hooks)
             (add-hook 'write-contents-hooks 'java-mode-untabify)))

;; Highlighter
(global-hl-line-mode 1)
(set-face-background 'hl-line "#383830")

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

(require 'color-theme-chocolate-rain)
(color-theme-chocolate-rain)

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
;; (define-key global-map (kbd "RET") 'newline-and-indent)

(keyboard-translate ?\C-h ?\C-?)                  ; assign backspace's abilities to C-h
(global-set-key [(meta h)] 'backward-kill-word)   ; this new key binding replaces mark-paragraph
(global-set-key [(hyper h)] 'help-command)        ; doesn't seem to be working

(setq inhibit-splash-screen t)          ; Disable splash screen
(setq initial-scratch-message nil)      ; Disable startup message
(delete-selection-mode t)               ; Enable C-D to delete selected text
(transient-mark-mode t)                 ; Enable typing to replace selected text

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

;; Set custom fame height and width
(set-frame-width-interactive 185)
(set-frame-height-interactive 86)

(server-start)
