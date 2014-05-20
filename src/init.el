;; init.el --- Master emacs initialisation file
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


;; ----- Includes
(add-to-list 'load-path "/usr/src")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/packages")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/elpa")

;; setup and load ELPA packages (and others)
(require 'package)
(setq package-user-dir "/usr/share/emacs/site-lisp/elpa")

;; add additional archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

;; requires
;; - core:
(require 'uniquify)
(require 'ido)
(require 'recentf)
(require 'server)
(require 'paren)

;; - from ELPA:
(require 'buffer-move)
(require 'fill-column-indicator)

;; common includes:
(load-library "init/libcommon")
(load-library "init/init-programming")
(load-library "init/init-mail")
(load-library "init/window-extra")

;; /usr/src
(load-library "timeclox.git/src/timeclox")

;; ----- Org mode
;; load git version, if it exists
(when (file-exists-p "/usr/src/org-mode.git/lisp/org-loaddefs.el")
  (add-to-list 'load-path "/usr/src/org-mode.git/lisp")
  (load-library "org-loaddefs.el"))

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

;; Turn on indent and visual line modes by default
(add-hook 'org-mode-hook '(lambda ()
                            (org-indent-mode 1)
                            (visual-line-mode)
                            (auto-fill-mode -1)))

;; ----- Aliases
(defalias 'hscroll-mode 'toggle-truncate-lines)
;; Alias query functions so all the replace functions start with "replace"!
(defalias 'replace-query 'query-replace)
(defalias 'replace-query-regexp 'query-replace-regexp)
(defalias 'replace-query-regexp-eval 'query-replace-regexp-eval)

;; ----- Key bindings
;; Awesome key bindings for moving around and manipulating windows.
(global-set-key [M-left]        'windmove-left)
(global-set-key [M-right]       'windmove-right)
(global-set-key [M-up]          'windmove-up)
(global-set-key [M-down]        'windmove-down)
(global-set-key (kbd "C-x p")   'other-previous-window)

;; Define miscellaneous shortcuts.
(global-set-key [f2]          'shell)
(global-set-key [f3]          'next-error)
(global-set-key [S-f3]        'previous-error)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

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
(global-set-key (kbd "C-x g")         'find-grep)
(global-set-key (kbd "C-x /")         'bury-buffer)
(global-set-key (kbd "C-x 4 k")       'kill-other-buffer)
(global-set-key (kbd "C-x 4 /")       'bury-other-buffer)

(global-set-key (kbd "C-c C-t")       'c-toggle-hungry-state)

;; redefine C-h (help) as C-x h and define backspace as C-h
(global-set-key (kbd "C-h")           'delete-backward-char)
(global-set-key (kbd "C-x h")         'help-command)

;; this new key binding replaces mark-paragraph
(global-set-key (kbd "M-h")           'backward-kill-word)   

;; ----- Settings
(setq inhibit-splash-screen   t         ; Disable splash screen
      initial-scratch-message nil)      ; Disable startup message
(delete-selection-mode t)               ; Enable C-D to delete
                                        ; selected text
(transient-mark-mode t)                 ; Enable typing to replace
                                        ; selected text

(setq uniquify-buffer-name-style 'forward)
(ido-mode t)

(put 'upcase-region 'disabled nil)


;; enable useful commands
(put 'narrow-to-region  'disabled nil)
(put 'erase-buffer      'disabled nil)

;; fci-mode
(setq-default fci-rule-color "gray9")

;; setup recentf-mode
(setq recentf-auto-cleanup    'never
      recentf-max-menu-items  25)

(global-set-key "\C-x\ r" 'recentf-open-files)
(recentf-mode 1)

;; the following fucks with certain major modes (i.e. yasnippet) and must be
;; disabled at all times
(setq mode-require-final-newline nil)

;; set text-mode as default mode
(setq default-major-mode 'text-mode)

;; Setup default browser - currently set to `xdg-open'.
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program  "xdg-open")

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
 column-number-mode           t
 comment-multi-line           t
 comment-style                (quote multi)
 indent-tabs-mode             nil
 standard-indent              2
 tab-width                    2
 fill-column                  80
 )

;; customise ANSI colours for shell-mode
(setq ansi-color-names-vector ["black" "red" "lawn green" "yellow" "cornflower blue"
                               "magenta" "cyan" "white"]
      ansi-color-map          (ansi-color-make-color-map))

;; set tab-stop positions for C-i
(setq tab-stop-list
      (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40
                42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76
                78 80 82 84 86 88 90 92 94 96 98 100)))

;; Following list of buffers shouldn't open a new window
(setq same-window-buffer-names '("*shell*"
                                 "*mail*"
                                 "*unsent mail*"
                                 "*info*"))

;; ----- Defuns used by conf files
;; Workaround for fci-mode.  Don't enable fci-mode if emacs version is 24.3
;; since a bug was introduced affecting cursor navigation.
(defun enable-fci-mode ()
  (interactive)
  (if (and (eq emacs-major-version 24)
           (eq emacs-minor-version 3))
      (fci-mode -1)
    (fci-mode)))

;; ----- Setup customisation of major modes.
(defun initialise-common-text ()
  (enable-fci-mode)                 ; fill column indicator
  (auto-fill-mode)                  ; auto fill
  (flyspell-mode)                   ; turn spell check on
  (abbrev-mode -1)                  ; turn abbrev-mode off
  )

(add-hook 'text-mode-hook 'initialise-common-text)

;; Change mode line background colour if file being edited as root.
(add-hook 'after-change-major-mode-hook 'warn-when-root-visits-file)

;; Let's make sure we disable linum-mode when in speedbar-mode
(defun initialise-linum-mode()
  (if (string= major-mode "speedbar-mode")
      (linum-mode -1)))

(add-hook 'linum-before-numbering-hook 'initialise-linum-mode)

;; ----- Load configurations specific to the environment
(load-library "init/init-x")
(load-library "init/init-term")

;; ----- Setup delayed initialisations
;; Start the server after 2 seconds have elapsed.
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

;; Show how long it took to initialise emacs after 6 seconds
(run-at-time "6 sec" nil
             '(lambda ()
                (message "init took %s" (emacs-init-time))))

;; Setup global timeclox mode to turn on after 10 seconds
(run-at-time "10 sec" nil
             '(lambda ()
                (unless global-timeclox-mode
                  (global-timeclox-mode 1))))
