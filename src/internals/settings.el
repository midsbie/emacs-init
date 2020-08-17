;;; settings.el --- Configures settings of emacs' internal features

;; Copyright (C) 2015-2018  Miguel Guedes

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

;;; Code:

(setq inhibit-splash-screen   t         ; Disable splash screen
      initial-scratch-message nil       ; Disable startup message
      password-cache-expiry   nil)      ; Disable password cache expiration

(delete-selection-mode t)               ; Enable C-D to delete selected text
(transient-mark-mode t)                 ; Enable typing to replace
                                        ; selected text

;; Set higher threshold before GC kicks in. Changing this setting seems to make
;; emacs snappier for some specific workflows.
(setq gc-cons-threshold 20000000)

;; enable useful commands
(put 'narrow-to-region          'disabled nil)
(put 'erase-buffer              'disabled nil)
(put 'upcase-region             'disabled nil)
(put 'downcase-region           'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; the following fucks with certain major modes (i.e. yasnippet) and must be
;; disabled at ALL times
(setq mode-require-final-newline nil)

;; set text-mode as default mode
(setq default-major-mode 'text-mode)

(menu-bar-mode -1)                      ; disable menu bar
; (size-indication-mode)                  ; turn on size indication mode

;; Note that `scroll-bar-mode' doesn't seem to be defined in emacs v24.5.1 and
;; possibly earlier versions.
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))                 ; disable scrollbars

(setq diff-switches         '-u         ; set diff to use unified format
      echo-keystrokes       0.1)        ; show keystrokes as they're happen

(fset 'yes-or-no-p 'y-or-n-p)           ; accept 'y' or 'n' instead of yes/no

(setq-default
 enable-recursive-minibuffers t         ; allow recursive editing in minibuffer
 column-number-mode           t
 comment-multi-line           t
 comment-style                'multi
 indent-tabs-mode             nil
 standard-indent              2
 tab-width                    2
 fill-column                  init/defaults/fill-column
 )

;; Set tab-stop positions for C-i at two characters wide.
(let (p)
  (dotimes (i 50)
    (setq p (cons (* 2 i) p)))
  (setq tab-stop-list (reverse p)))

;; Following list of buffers shouldn't open a new window
(setq same-window-buffer-names '("*shell*"
                                 "*mail*"
                                 "*unsent mail*"
                                 "*info*"))

;; Do not create lock files and place backup files in a `backups` directory.
(setq create-lockfiles nil)

(setq backup-directory-alist `(("." . "~/.backups"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Windows
(setq-default split-height-threshold 100)

;; Revert buffer if file associated with it changes outside of emacs.
;; (global-auto-revert-mode 1)

;; Run a MAJORMODE-local-vars-hook when local vars are processed.
;; From: https://www.emacswiki.org/emacs/LocalVariables
(add-hook 'hack-local-variables-hook 'run-local-vars-mode-hook)
(defun run-local-vars-mode-hook ()
  "Run a hook for the MAJOR-MODE.

This runs after the local variables have been processed."
  (run-hooks (intern (concat (symbol-name major-mode) "-local-vars-hook"))))

(defun apply-editor-workarounds()
"Set correct `C-j` mechanics for current Emacs version

From 24.3 onwards, the behaviours of the `RET` and `C-j` keys
were swapped.  We don't like that.  This defun is supposed to be
invoked by mode initialisors, in particular the programming ones
and makes RET and C-j work correctly.

It would now seem that version 26.x restores pre-24.3 behaviour."
  (when (and (>= emacs-major-version 24)
             (>= emacs-minor-version 3))
    (local-set-key (kbd "RET") 'electric-newline-and-maybe-indent)
    (local-set-key (kbd "C-j") 'newline)))

(defun detach-editor-workarounds()
  "Undo side-effects from running `apply-editor-workarounds'"
  (local-unset-key (kbd "RET"))
  (local-unset-key (kbd "C-j")))

;;; settings.el ends here
