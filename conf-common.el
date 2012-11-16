;; includes
(load-library "./libcommon")

(load-library "./conf-cedet")
(load-library "./conf-frame-size")
(load-library "./conf-mail")
(load-library "./conf-compile")

(load-library "./buffer-move")
(load-library "./framemove")
(load-library "./dedicated-window")

;; requires
(require 'uniquify)
(require 'ido)
(require 'recentf)
(require 'server)

;; setup recentf-mode
(setq recentf-auto-cleanup 'never);
(recentf-mode 1)

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

;; set c-mode default style and tabs
(setq c-default-style "linux"
      c-basic-offset 2
      tab-width 2)
(setq-default indent-tabs-mode nil)

(setq default-major-mode 'text-mode)    ; set text-mode as default mode

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

;; buffer related
(global-set-key (kbd "C-x x")         'mark-whole-buffer)
(global-set-key (kbd "C-S-w")         'toggle-truncate-lines)
(global-set-key (kbd "M-r")           'revert-buffer)
(global-set-key (kbd "C-x C-b")       'ibuffer)

(global-set-key (kbd "C-c C-t")       'c-toggle-hungry-state)

;; redefine C-h (help) as C-x h and define backspace as C-h
(keyboard-translate ?\C-h ?\C-?)
(global-set-key (kbd "C-h")           'delete-backward-char)
(global-set-key (kbd "C-x h")         'help)

;; this new key binding replaces mark-paragraph
(global-set-key (kbd "M-h")           'backward-kill-word)   

(setq inhibit-splash-screen t           ; Disable splash screen
      initial-scratch-message nil)      ; Disable startup message
(delete-selection-mode t)               ; Enable C-D to delete selected text
(transient-mark-mode t)                 ; Enable typing to replace selected text

(setq uniquify-buffer-name-style 'forward)
(ido-mode t)

(put 'upcase-region 'disabled nil)

(menu-bar-mode -1)                      ; disable menu bar
(size-indication-mode)                  ; turn on size indication mode
(global-linum-mode t)
(scroll-bar-mode -1)                    ; disable scrollbars

(setq-default
 enable-recursive-minibuffers t         ; allow recursive editing in minibuffer
 column-number-mode t
 comment-multi-line t
 comment-style (quote align)
 css-electric-brace-behavior nil
 css-indent-offset 2
 indent-tabs-mode nil
 js-indent-level 2
 standard-indent 2
 tab-width 2)

(setq tab-stop-list (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90 92 94 96 98 100)))
(split-window-horizontally)             ; two windows at startup

; Start server if it isn't running yet
(if (not (server-running-p))
    (progn
      (message "[server] starting")
      (server-start))
  (message "[server] already started: not starting"))

(load-library "./conf-x")
