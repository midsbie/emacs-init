;;; settings.el --- Configures settings of emacs' internal features

;; Copyright (C) 2015-2023  Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This setting did NOT prevent Emacs from aggressively resizing windows.
;; Ref: https://stackoverflow.com/a/11458625
;; (setq even-window-heights nil)

;;

;;; Code:

;; Set path override for Mono libraries or the Omnisharp Roslyn server may not
;; start or work as expected.
(setenv "FrameworkPathOverride" "/lib/mono/4.5")

;; GENERAL CONFIGURATION
;; -----------------------------------------------------------------------------
;; Emacs initialization
(setq inhibit-splash-screen   t         ; Disable splash screen
      initial-scratch-message nil       ; Disable startup message
      )

;; Assorted settings
(setq-default
 enable-recursive-minibuffers t         ; allow recursive editing in minibuffer
 help-window-select           'other    ; focus on help window when spawning
 truncate-lines               t         ; don't wrap, truncate lines by default
 idle-update-delay            1.1       ; reduce rate at which UI updates
 echo-keystrokes              0.2       ; show keystrokes as they're happen
 ring-bell-function           'ignore   ; don't ring any bell
 ;; Recommendation from https://protesilaos.com/emacs/modus-themes
 ;; found in: https://github.com/jeremyf/dotemacs/blob/main/emacs.d/configuration.org
 x-underline-at-descent-line  t
 password-cache-expiry        nil       ; disable password cache expiration
 save-place                   t         ; save position in buffer
 uniquify-buffer-name-style   'forward
 )

;; This measure is needed to prevent tramp from hanging at startup as it tries
;; to conduct a strange check on the system's host.
;;
;; More information here: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20015
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ConnectTimeout=1 -o ControlPersist=no")

;; Modes
(setq default-major-mode 'text-mode     ; set text-mode as default mode
      ;; The following fucks with certain major modes (i.e. yasnippet) and must
      ;; be disabled at ALL times
      mode-require-final-newline nil
      )

;; Column
(setq-default column-number-mode    t
              fill-column           init/defaults/fill-column)

;; Indentation
(setq-default indent-tabs-mode      nil
              standard-indent       2
              tab-width             2)

;; Coding-related
(setq-default comment-multi-line    t
  comment-style                     'multi
  diff-switches                     '-u ; set diff to use unified format
  vc-follow-symlinks                t   ; follow symlinks instead of prompting
  )

;; Backups
(setq backup-directory-alist        `(("." . "~/.emacs.d/backups"))
      backup-by-copying             t   ; place backup files in `backups`
      create-lockfiles              nil ; do not create lock files
      delete-old-versions           t
      kept-new-versions             20
      kept-old-versions             5
      version-control t                 ; use version numbers on backups
      )

;; Set calendar's week start day to Monday and not Sunday, as by default.
(setq calendar-week-start-day 1)

;; Set default browser - currently set to `xdg-open'.
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program  "xdg-open")

;; Following list of buffers shouldn't open a new window
(setq same-window-buffer-names '("*shell*"
                                 "*mail*"
                                 "*unsent mail*"
                                 "*info*"))

;; Preferred coding system
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Mode set up
(delete-selection-mode t)               ; C-D deletes selected text
(transient-mark-mode t)                 ; typing replaces selected text
(global-so-long-mode t)                 ; better handling of long files
(size-indication-mode t)                ; size indication mode
(global-prettify-symbols-mode)          ; (lambda ... -> (λ ...
(winner-mode)                           ; window configuration mutation undo

(menu-bar-mode -1)                      ; disable menu bar
(tool-bar-mode -1)                      ; disable toolbar
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
;; The following settings as per the documentation on improving the performance
;; of LSP at:
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;;
;; Set higher threshold before GC kicks in. Changing this setting seems to make
;; emacs snappier for some specific workflows.
;;
;; It may be wise to set the GC threshold to a reasonable value or we may end up
;; hindering performance.  Might be best to have more frequent clean ups taking
;; an imperceptible amount of time to complete, rather less frequent ones that
;; momentarily block editing.
;;
;; Noting that on Emacs 29.0.90 it no longer seems possible to set this value.
(setq gc-cons-threshold (* 16 1024 1024)       ; 16 MiB
      )

;; Documentation of `read-process-output-max' states that "on GNU/Linux systems,
;; the value should not exceed /proc/sys/fs/pipe-max-size".
(setq read-process-output-max (with-temp-buffer
                                (insert-file-contents "/proc/sys/fs/pipe-max-size")
                                (string-to-number (buffer-string))))

;; Some settings that may help with redisplay
;; Ref: [4:25] https://200ok.ch/posts/2020-10-01_introduction_to_profiling_in_emacs.html
(setq bidi-paragraph-direction  'left-to-right
      bidi-inhibit-bpa          t)

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
  (setq tab-stop-list (reverse p)))

;; Note that `scroll-bar-mode' doesn't seem to be defined in emacs v24.5.1 and
;; possibly earlier versions.
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))                 ; disable scrollbars

(fset 'yes-or-no-p 'y-or-n-p)           ; accept 'y' or 'n' instead of yes/no

;; To make it easier for newcomers to Emacs, the developers decided to swap out
;; the mechanics of C-j and RET when `electric-indent-mode' is enabled.  In the
;; hook below, we make sure to retain the desired behaviour whereby C-j
;; produces a newline and indention and C-m or RET only a newline.
;;
;; Ref: https://lists.gnu.org/archive/html/bug-gnu-emacs/2014-12/msg00098.html
(add-hook 'electric-indent-mode-hook
          #'(lambda ()
              (if electric-indent-mode
                  (progn
                    (global-set-key (kbd "C-j") 'newline)
                    (global-set-key (kbd "RET") 'electric-newline-and-maybe-indent))
                (global-set-key (kbd "C-j") 'electric-newline-and-maybe-indent)
                (global-set-key (kbd "RET") 'newline))))
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

;; Windows
(setq-default split-height-threshold 100)

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

;;; settings.el ends here
