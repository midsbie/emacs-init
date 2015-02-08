;;; settings.el --- Configures settings of emacs' internal features

;; Copyright (C) 2015  Miguel Guedes

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

(delete-selection-mode t)               ; Enable C-D to delete
                                        ; selected text
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

(scroll-bar-mode -1)                    ; disable scrollbars
(setq diff-switches         '-u         ; set diff to use unified format
      echo-keystrokes       0.1)        ; show keystrokes as they're happen

(fset 'yes-or-no-p 'y-or-n-p)           ; accept 'y' or 'n' instead of yes/no

(setq-default
 enable-recursive-minibuffers t         ; allow recursive editing in
                                        ; minibuffer
 column-number-mode           t
 comment-multi-line           t
 comment-style                'multi
 indent-tabs-mode             nil
 standard-indent              2
 tab-width                    2
 fill-column                  80
 )

;; Set tab-stop positions for C-i
(setq tab-stop-list
      (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40
                42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76
                78 80 82 84 86 88 90 92 94 96 98 100)))

;; Following list of buffers shouldn't open a new window
(setq same-window-buffer-names '("*shell*"
                                 "*mail*"
                                 "*unsent mail*"
                                 "*info*"))

;;; settings.el ends here
