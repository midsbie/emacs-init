;; includes
(add-to-list 'load-path "/usr/src")

(load-library "./libcommon")

(load-library "./conf-programming")
(load-library "./conf-frame-size")
(load-library "./conf-mail")

(load-library "./buffer-move")
(load-library "./framemove")
(load-library "./window-extra")
(load-library "fill-column-indicator.git/fill-column-indicator")
(load-library "timeclox.git/timeclox")

;; requires
(require 'uniquify)
(require 'ido)
(require 'recentf)
(require 'server)
(require 'paren)
(require 'highlight-parentheses)

;; enable useful commands
(put 'narrow-to-region  'disabled nil)
(put 'erase-buffer      'disabled nil)

;; fci-mode
(setq-default fci-rule-color "gray9")

;; setup recentf-mode
(setq recentf-auto-cleanup 'never);
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ r" 'recentf-open-files)
(recentf-mode 1)

;; the following fucks with certain major modes (i.e. yasnippet) and must be
;; disabled at all times
(setq mode-require-final-newline nil)

;; load cc-mode
(autoload 'awk-mode "cc-mode" nil t)
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-toggle-auto-hungry-state 1)
            (c-toggle-auto-state -1)
            (c-set-offset 'innamespace 0)           ; set indentation inside
                                                    ; namespaces to nil
          ))

;; set c-mode default style and tabs
(setq c-default-style "linux"
      c-basic-offset  2
      tab-width       2)
(setq-default indent-tabs-mode nil)

(setq default-major-mode 'text-mode)    ; set text-mode as default mode

;; Setup default browser - currently set to `xdg-open'.
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "xdg-open")

;; ---- Org mode ----
(setq org-use-fast-todo-selection t)

(setq org-todo-keyword-faces
      '(("TODO"  . (:foreground "red" :weight bold))
        ("NEXT"  . (:foreground "red" :weight bold))
        ("DONE"  . (:foreground "forest green" :weight bold))
        ("WAITING"  . (:foreground "orange" :weight bold))
        ("CANCELLED"  . (:foreground "forest green" :weight bold))
        ("SOMEDAY"  . (:foreground "orange" :weight bold))
        ("OPEN"  . (:foreground "red" :weight bold))
        ("CLOSED"  . (:foreground "forest green" :weight bold))
        ("ONGOING"  . (:foreground "orange" :weight bold))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
        (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")
        (sequence "SOMEDAY(s!/!)" "|")
        (sequence "OPEN(O!)" "|" "CLOSED(C!)")
        (sequence "ONGOING(o!/!)" "|")))

;; ---- Aliases ----
(defalias 'hscroll-mode 'toggle-truncate-lines)
;; Alias query functions so all the replace functions start with "replace"!
(defalias 'replace-query 'query-replace)
(defalias 'replace-query-regexp 'query-replace-regexp)
(defalias 'replace-query-regexp-eval 'query-replace-regexp-eval)

;; ---- Key bindings ----
;; windmove: awesome key bindings for moving around windows
(global-set-key [M-left]        'windmove-left)
(global-set-key (kbd "C-x p")   'other-previous-window)
(global-set-key [M-right]       'windmove-right)
(global-set-key [M-up]          'windmove-up)
(global-set-key [M-down]        'windmove-down)

;; define miscellaneous shortcuts
(global-set-key [f2]          'shell)
(global-set-key [f3]          'next-error)
(global-set-key [S-f3]        'previous-error)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
;; (global-set-key [S-f4]      'delete-other-windows)
;; (global-set-key [f6]        'other-window)

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
(global-set-key (kbd "C-x /")         'bury-buffer)
(global-set-key (kbd "C-x g")         'find-grep)

(global-set-key (kbd "C-c C-t")       'c-toggle-hungry-state)

;; redefine C-h (help) as C-x h and define backspace as C-h
(global-set-key (kbd "C-h")           'delete-backward-char)
(global-set-key (kbd "C-x h")         'help-command)

;; this new key binding replaces mark-paragraph
(global-set-key (kbd "M-h")           'backward-kill-word)   

(setq inhibit-splash-screen t           ; Disable splash screen
      initial-scratch-message nil)      ; Disable startup message
(delete-selection-mode t)               ; Enable C-D to delete
                                        ; selected text
(transient-mark-mode t)                 ; Enable typing to replace
                                        ; selected text
;(electric-indent-mode t)                ; set electric indentation mode

(setq uniquify-buffer-name-style 'forward)
(ido-mode t)

(put 'upcase-region 'disabled nil)

(menu-bar-mode -1)                      ; disable menu bar
(size-indication-mode)                  ; turn on size indication mode
(global-linum-mode t)                   ; display line numbers in left margin
(scroll-bar-mode -1)                    ; disable scrollbars
(show-paren-mode t)                     ; show parenthesis next to cursor

(setq show-paren-mismatch t             ; show parenthesis mismatch
      diff-switches       '-u           ; set diff to use unified format
      save-place          t)            ; save position on buffer

(fset 'yes-or-no-p 'y-or-n-p)           ; accept 'y' or 'n' instead of yes/no

(setq-default
 enable-recursive-minibuffers t         ; allow recursive editing in
                                        ; minibuffer
 column-number-mode t
 comment-multi-line t
 comment-style (quote align)
 css-electric-brace-behavior nil
 css-indent-offset 2
 indent-tabs-mode nil
 js-indent-level 2
 standard-indent 2
 tab-width 2
 fill-column 80
 )

;; customise ANSI colours for shell-mode
(setq ansi-color-names-vector ["black" "red" "lawn green" "yellow" "cornflower blue"
                               "magenta" "cyan" "white"]
      ansi-color-map          (ansi-color-make-color-map))

;; let's setup text-mode to our liking
(add-hook 'text-mode-hook '(lambda()
                             (enable-fci-mode)
                             (auto-fill-mode)
                             (turn-on-flyspell)))


;; set tab-stop positions for C-i
(setq tab-stop-list
      (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40
                42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76
                78 80 82 84 86 88 90 92 94 96 98 100)))

;; Let's make sure we disable linum-mode when in speedbar-mode
(defun conf-linum-mode-hook()
  (if (string= major-mode "speedbar-mode")
      (linum-mode -1)))

(add-hook 'linum-before-numbering-hook 'conf-linum-mode-hook)

;; Following list of buffers shouldn't open a new window
(setq same-window-buffer-names '("*shell*"
                                 "*mail*"
                                 "*unsent mail*"
                                 "*info*"))

;; Start the server after 2 seconds have elapsed.
;; NOTE: not used anymore as the server issues have been solved - CEDET was
;; found to be the culprit.
(defun server-start-timed()
    (run-at-time "2 sec" nil
                 '(lambda ()
                    (message "[server] starting")
                    (server-start)))
    )

;; Start server if it isn't running yet.
;; NOTE: strangely the call to server-start needs to be issued a few seconds
;; after emacs has launched.
(if (not (server-running-p))
    (server-start-timed)
  (message "[server] already started: not starting"))

;; Disable 'buffer * still has clients' message shown when killing buffers
;; spawned by emacsclient.
;; NOTE: for some reason the call to remove-hook needs to take place a few
;; seconds after emacs has launched.
(run-at-time "3 sec" nil
             '(lambda ()
                (remove-hook 'kill-buffer-query-functions
                             'server-kill-buffer-query-function)))

;; Display a warning signal in the mode line when visiting a file with root
;; privileges.
(defun root-file-warning ()
  (when (string-match "^/su\\(do\\)?:" default-directory)
    (face-remap-add-relative
     'mode-line
     '(:background "red3" :foreground "white"))
    (face-remap-add-relative
     'mode-line-inactive
     '(:background "red4" :foreground "dark gray"
                   :box nil))
;;    (server-start-timed)
    )
  )

;; Workaround for fci-mode.  Don't enable fci-mode if emacs version is 24.3
;; since a bug was introduced affecting cursor navigation.
(defun enable-fci-mode ()
  (interactive)
  (if (and (eq emacs-major-version 24)
           (eq emacs-minor-version 3))
      (fci-mode -1)
    (fci-mode)))

(add-hook 'find-file-hook   'root-file-warning)
(add-hook 'dired-mode-hook  'root-file-warning)

;; now load X-specific configuration
(load-library "./conf-x")

;; Show how long it took to initialise emacs after 6 seconds
(run-at-time "6 sec" nil
             '(lambda ()
                (message "init took %s" (emacs-init-time))))

(run-at-time "10 sec" nil
             '(lambda ()
                (unless global-timeclox-mode
                  (global-timeclox-mode 1))))