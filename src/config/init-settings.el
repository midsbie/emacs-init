;;; init-settings.el --- Core editor settings  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defvar init/compilation-jump-to-bottom t
  "When non-nil, move point to the end of a visible compilation buffer on start.")

;; Prevent Git from using pager in `shell-mode'.
(setenv "GIT_PAGER" "cat")

;; GENERAL CONFIGURATION
;; -----------------------------------------------------------------------------
;; Emacs initialization
(setopt inhibit-startup-screen   t      ; Disable splash screen
        initial-scratch-message nil     ; Disable startup message
      )

;; Assorted settings
(setopt
 enable-recursive-minibuffers t         ; allow recursive editing in minibuffer
 truncate-lines               t         ; don't wrap, truncate lines by default
 echo-keystrokes              0.2       ; show keystrokes as they're happen
 ring-bell-function           'ignore   ; don't ring any bell
 ;; Recommendation from https://protesilaos.com/emacs/modus-themes found in:
 ;; https://github.com/jeremyf/dotemacs/blob/main/emacs.d/configuration.org
 x-underline-at-descent-line  t
 )

(setopt uniquify-buffer-name-style 'forward
        password-cache-expiry        nil  ; disable password cache expiration
        tree-widget-image-enable nil      ; text symbols for tree widgets
)

;; Column
(column-number-mode 1)
(setopt fill-column init/defaults/fill-column)

;; Indentation
(setopt indent-tabs-mode nil
        standard-indent  2
        tab-width        2)

;; Coding-related
(setopt comment-multi-line t
        comment-style     'multi-line
        vc-follow-symlinks t)          ; follow symlinks instead of prompting

;; Backups
(setopt backup-directory-alist `(("." . "~/.emacs.d/backups"))
        backup-by-copying      t       ; place backup files in `backups`
        delete-old-versions    t
        kept-new-versions      20
        kept-old-versions      5
        version-control        t)      ; use version numbers on backups
(setopt create-lockfiles nil)          ; do not create lock files

;; Set calendar's week start day to Monday and not Sunday, as by default.
(setopt calendar-week-start-day 1)

;; Set default browser - currently set to `xdg-open'.
(setopt browse-url-browser-function 'browse-url-generic
        browse-url-generic-program  "xdg-open")

;; Preferred coding system
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Mode set up
(delete-selection-mode t)               ; C-D deletes selected text
(transient-mark-mode t)                 ; typing replaces selected text
(global-so-long-mode t)                 ; better handling of long files
;; Mode-selection alists run after explicit mode cookies and shebangs; intercept
;; selection itself so large files skip language hooks in every case.
(advice-add 'set-auto-mode :before-until #'my/handle-large-file-hook)

(size-indication-mode t)                ; size indication mode
(global-prettify-symbols-mode)          ; (lambda ... -> (λ ...
(save-place-mode 1)                     ; save position in buffer

(tooltip-mode -1)                       ; disable tooltips

;; Enable useful commands
(put 'narrow-to-region          'disabled nil)
(put 'erase-buffer              'disabled nil)
(put 'upcase-region             'disabled nil)
(put 'downcase-region           'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; NATIVE COMPILATION OPTIMIZATIONS
;; -----------------------------------------------------------------------------
;; - Docs for `native-comp-speed' states that a speed of 3 can lead to
;;   "dangerous" optimizations.
;;
;; - Some users reported that passing "-O3" to the native compiler can lead to
;;   worse performance.
;;
;; This has been disabled for now until native compilation matures further.
;;
;; (when 'native-comp-compiler-options
;;   (setq native-comp-speed 3
;;         native-comp-compiler-options
;;         '("-O3"
;;           ;; This is not understood by gcc on the main machine:
;;           ;; "-march=native"
;;           ;;
;;           ;; Using the following instead as per:
;;           "-m64"
;;           "-mtune=native")))

;; PERFORMANCE OPTIMIZATIONS
;; -----------------------------------------------------------------------------
;; `gc-cons-threshold' is managed by `gcmh-mode' (see features/init-gcmh.el).
;;
;; Documentation of `read-process-output-max' states that "on GNU/Linux systems,
;; the value should not exceed /proc/sys/fs/pipe-max-size".
(setq read-process-output-max (with-temp-buffer
                                (insert-file-contents "/proc/sys/fs/pipe-max-size")
                                (string-to-number (buffer-string))))

;; Some settings that may help with redisplay
;; Ref: [4:25] https://200ok.ch/posts/2020-10-01_introduction_to_profiling_in_emacs.html
(setopt bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; The `list-timers' command is super useful when debugging high CPU usage
;; where timers, idle or otherwise, are involved, but for some reason it is
;; disabled.  Also of note are the `timer-list' and `timer-idle-list' lists.
;;
;; See this post I created for useful context:
;; https://emacs.stackexchange.com/q/70926/
(put 'list-timers 'disabled nil)

;; Set tab-stop positions for C-i at two characters wide.
(let (p)
  (dotimes (i 50)
    (setq p (cons (* 2 i) p)))
  (setopt tab-stop-list (reverse p)))

(setopt use-short-answers t)

;; To make it easier for newcomers to Emacs, the developers decided to swap out
;; the mechanics of C-j and RET when `electric-indent-mode' is enabled.  Retain
;; the desired behaviour whereby C-j produces a newline with indentation, and
;; C-m or RET only a newline, regardless of whether electric indentation is
;; enabled in the buffer.
;;
;; Ref: https://lists.gnu.org/archive/html/bug-gnu-emacs/2014-12/msg00098.html
(global-set-key (kbd "RET") #'electric-indent-just-newline)
(global-set-key (kbd "C-j") #'newline-and-indent)
(electric-indent-mode 1)
;; Also enabling `electric-pair-mode' because it works great with indent above.
;; Note that pair requires indent or indentation will be missing in some
;; instances.
;;
;; 230914 This has now been disabled because it isn't very smart at knowing when
;;        NOT to automatically add a closing pair.
(electric-pair-mode -1)
;; This is problematic in some modes like `typescript-mode', as it forcefully
;; adds newlines when some characters are types (e.g. `{').
(electric-layout-mode -1)

;; Do not allow the cursor in the minibuffer prompt
(setopt minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

;; Run a MAJORMODE-local-vars-hook when local vars are processed.
;; From: https://www.emacswiki.org/emacs/LocalVariables
(defun my/run-local-vars-mode-hook ()
  "Run a hook for the MAJOR-MODE.

This runs after the local variables have been processed."
  (run-hooks (intern (concat (symbol-name major-mode) "-local-vars-hook"))))

(add-hook 'hack-local-variables-hook #'my/run-local-vars-mode-hook)

(defun my/compilation-started (proc)
  "Move the cursor to the bottom of the buffer associated to the
process PROC at the start of a compilation."
  (let* ((buf (process-buffer proc))
         (win (get-buffer-window buf 'visible)))
    (when win
      (with-selected-window win
        (when (and init/compilation-jump-to-bottom
                   (eq major-mode 'compilation-mode))
          (goto-char (point-max)))))))

(add-hook 'compilation-start-hook #'my/compilation-started)

(provide 'init-settings)

;;; init-settings.el ends here
