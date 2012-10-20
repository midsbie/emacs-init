;; requires
(require 'conf-cedet)
(require 'conf-frame-size)
(require 'buffer-move)
(require 'framemove)
(require 'uniquify)
(require 'ido)
(require 'dedicated-window)


;; load cc-mode
(autoload 'awk-mode "cc-mode" nil t)
(add-hook 'c-mode-common-hook
          (lambda () (c-toggle-auto-hungry-state 1)
            (c-toggle-auto-state -1)))

;; nxhtml-mode
;; (load "nxhtml/autostart.el")
;; (setq mumamo-background-colors nil)     ;; disable background color changes
;; (css-color-global-mode)

;; (defun web-mode()
;;   (interactive)
;;   (nxhtml-mumamo-mode))

;; (add-to-list 'auto-mode-alist '("\\.inc$" . nxhtml-mumamo-mode))
;; (add-to-list 'auto-mode-alist '("\\.php$" . nxhtml-mumamo-mode))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; FIX for mumamo's annoying warning messages ;;;;;;;;;;;;;;;;
;; ;; Mumamo is making emacs 23.3 freak out:
;; (when (and (equal emacs-major-version 24)
;;            (equal emacs-minor-version 2))
;;   (eval-after-load "bytecomp"
;;     '(add-to-list 'byte-compile-not-obsolete-vars
;;                   'font-lock-beginning-of-syntax-function))
;;   ;; tramp-compat.el clobbers this variable!
;;   (eval-after-load "tramp-compat"
;;     '(add-to-list 'byte-compile-not-obsolete-vars
;;                   'font-lock-beginning-of-syntax-function)))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(setq default-major-mode 'text-mode)    ; set text-mode as default mode

;; Enable clipboard functionality
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; ---- Aliases ----
(defalias 'hscroll-mode 'toggle-truncate-lines)
;;Alias query functions so all the replace functions start with "replace"!
(defalias 'replace-query 'query-replace)
(defalias 'replace-query-regexp 'query-replace-regexp)
(defalias 'replace-query-regexp-eval 'query-replace-regexp-eval)

;; windmove: awesome key bindings for moving around windows
(global-set-key [M-left]    'windmove-left)
(global-set-key [M-right]   'windmove-right)
(global-set-key [M-up]      'windmove-up)
(global-set-key [M-down]    'windmove-down)

;; buffer-move: key bindings
(global-set-key (kbd "<C-S-up>")      'buf-move-up)
(global-set-key (kbd "<C-S-down>")    'buf-move-down)
(global-set-key (kbd "<C-S-left>")    'buf-move-left)
(global-set-key (kbd "<C-S-right>")   'buf-move-right)

(global-set-key (kbd "C-x x")         'mark-whole-buffer)
(global-set-key (kbd "C-S-w")         'toggle-truncate-lines)
(global-set-key (kbd "M-r")           'revert-buffer)

;; (global-set-key (kbd "C-c C-t") 'c-toggle-hungry-state)

;; redefine C-h (help) as C-x h and define backspace as C-h
(keyboard-translate ?\C-h ?\C-?)
(global-set-key (kbd "C-h")           'delete-backward-char)
(global-set-key (kbd "C-x h")         'help)

;; this new key binding replaces mark-paragraph
(global-set-key (kbd "M-h")           'backward-kill-word)   

(setq inhibit-splash-screen t)          ; Disable splash screen
(setq initial-scratch-message nil)      ; Disable startup message
(delete-selection-mode t)               ; Enable C-D to delete selected text
(transient-mark-mode t)                 ; Enable typing to replace selected text

(setq uniquify-buffer-name-style 'forward)
(ido-mode t)

(setq frame-title-format "%b - emacs")
(put 'upcase-region 'disabled nil)

(size-indication-mode)                  ; turn on size indication mode
(scroll-bar-mode -1)                    ; disable scrollbars
(menu-bar-mode -1)                      ; disable menu bar

(setq enable-recursive-minibuffers t)   ; allow recursive editing in minibuffer
(split-window-horizontally)             ; two windows at startup

;; Setup NNTP newsgroups
(setq gnus-select-method '(nntp "eunews.blocknews.net")
      user-full-name "Miguel Guedes"
      user-mail-address "miguel.a.guedes@gmail.com"
      nntp-authinfo-file "~/.authinfo"
      gnus-read-active-file nil)

(setq gnus-secondary-select-methods
      '(nnimap "Personal"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com"
                                   587
                                   "miguel.a.guedes@gmail.com"
                                   nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "abstratti.com")

;; variables
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
 '(default ((t (:inherit nil :stipple nil :background "#080808" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 88 :width normal :foundry "unknown" :family "Liberation Mono")))))

;; Let's now perform last initialization steps
(interactive)
(add-to-list 'default-frame-alist (cons 'width 185))
(add-to-list 'default-frame-alist (cons 'height 73))
(set-frame-width-interactive 185)
(set-frame-height-interactive 73)

;; Start server if it isn't running yet
(if (and (fboundp 'server-running-p) 
         (not (server-running-p)))
    (server-start))

(provide 'conf-common)
