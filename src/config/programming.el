;;; programming.el --- Programming-related configuration

;; Copyright (C) 2015-2022  Miguel Guedes

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

;; Global programming settings
;; --
;; Own custom default style
(c-add-style "default"
             '("linux"
               (c-recognize-knr-p . nil)
               (c-basic-offset  . 2)
               (indent-tabs-mode . nil)
               (c-comment-only-line-offset . 0)
               (c-syntactic-indentation-in-macros . nil)
               (c-hanging-braces-alist . ((brace-list-open)
                                          (brace-entry-open)
                                          (substatement-open after)
                                          (block-close . c-snug-do-while)
                                          (arglist-cont-nonempty)))
               (c-cleanup-list . (brace-else-brace
                                  brace-elseif-brace
                                  brace-catch-brace
                                  empty-defun-braces
                                  one-liner-defun
                                  defun-close-semi
                                  ;;                               space-before-funcall
                                  compact-empty-funcall))
               (c-hanging-semi&comma-criteria . ((lambda () 'stop)))
               (c-offsets-alist . ((innamespace           . 0)
                                   (statement-block-intro . +)
                                   (knr-argdecl-intro     . 0)
                                   (substatement-open     . 0)
                                   (substatement-label    . 0)
                                   (label                 . 0)
                                   (statement-cont        . +)))))

;; TODO: Investigate how to make the following work with
;; `c-hanging-semi&comma-criteria above.  Tried and failed to get it to work and
;; in the end simply disabled newline on semi.
;;
;;                . (c-semi&comma-no-newlines-for-oneline-inliners
;;                   c-semi&comma-inside-parenlist
;;                   c-semi&comma-no-newlines-before-nonblanks))

;; No clean-ups.  May prove useful in chaotically formatted code.
;; NOTE: don't forget to also turn off `c-auto-newline'!
(c-add-style "default-bland"
             '("default"
               (c-cleanup-list . nil)))

;; OpenBSD style.
(c-add-style "openbsd"
             '("bsd"
               (c-backspace-function . delete-backward-char)
               (c-syntactic-indentation-in-macros . nil)
               (c-tab-always-indent . nil)
               (c-hanging-braces-alist (block-close . c-snug-do-while))
               (c-offsets-alist (arglist-cont-nonempty . *)
                                (statement-cont . *))
               (indent-tabs-mode . t)))

;; Default settings
(setq-default c-default-style    "default" ; own default style
              tab-width          2
              indent-tabs-mode   nil
              js-indent-level    2)

;; Set the extra indentation before a substatement (e.g. the opening brace in
;; the consequent block of an if statement) to 0 (instead of '+)
;; Ref: https://stackoverflow.com/a/3956173
(c-set-offset 'substatement-open 0)

;; Set environment for compilers to use
(when (executable-find "clang")
  (setenv "CC" "clang"))
(when (executable-find "clang++")
  (setenv "CXX" "clang++"))

;; Hooks
(add-hook 'c-mode-common-hook 'init/common-nonweb-programming-mode)
(add-hook 'c-mode-common-hook 'init/c-c++)
(add-hook 'minibuffer-mode-hook 'init/disable-electricity)
(add-hook 'conf-mode-hook 'init/disable-electric-indent)

;; Defuns
;; --
(defun init/disable-electricity()
  "Disable electric enhancements."
  (electric-pair-local-mode -1)
  (electric-layout-local-mode -1)
  (init/disable-electric-indent))

(defun init/disable-electric-indent()
  "Disable electric indentation behaviour."
  (electric-indent-local-mode -1))

(defun init/common-programming-mode ()
  ;; NOTE: we are explicitly disabling `flyspell-mode' since it suffers from
  ;; very annoying, intrusive issues that actually impede development.
  (setq-local programming-buffer t)

  ;; Providing navigation between code blocks designated by curly brackets and parentheses.
  (local-set-key (kbd "C-x [") 'backward-up-list)
  (local-set-key (kbd "C-x ]") 'up-list)
  (local-set-key (kbd "C-x {") 'backward-list)
  (local-set-key (kbd "C-x }") 'forward-list)

  ;; Delete all trailing whitespace before saving
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t) ; local hook
  (setq-local show-trailing-whitespace t))

(defun init/common-nonweb-programming-mode ()
  "Perform initialisation of aspects common to all programming-related modes."
  (auto-fill-mode)                  ; auto fill

;;(flyspell-prog-mode)              ; turn spell check for strings and comments
;;(ac-flyspell-workaround)          ; this defun must be executed to prevent
                                    ; flyspell from messing with
                                    ; auto-complete-mode

  (highlight-parentheses-mode)      ; turn on { } and ( ) highlighting
  (abbrev-mode -1)                  ; turn abbrev-mode off
  (subword-mode 1)

  ;; `c-auto-newline' was previously disabled and done so for a reason. Have
  ;; just tested it and seemed fine whilst editing a C source file but may not
  ;; work well on all major modes. If that's the case then it needs activation
  ;; on a per major-mode basis. However, if it needs to be disabled altogether,
  ;; please explain why for future reference.
  ;;
  ;; Update: disabled because it turns out to be too intrusive when editing
  ;; code.  It (mostly) works fine when writing new code though.
  (setq c-auto-newline nil)

  ;; The following disabled:
; (doxymacs-mode)                    ; turn doxymacs on
  (init/common-programming-mode))

(defun init/c-c++()
  "Initialise modes related to C and C++ development."
  (local-set-key "\C-co" 'buftoggle)

  (c-toggle-auto-hungry-state 1)
  (c-toggle-auto-state -1))

(defun init/common-web-programming-mode ()
  "Initialise modes related to web development."

  (auto-fill-mode -1)
  (highlight-parentheses-mode)
  (subword-mode 1)

  (setq-local fill-column init/defaults/fill-column)

  (setq-local tab-width   2)
  (setq-local c-basic-offset  2)

  ;; Add node_modules path to `exec-path' to enable:
  ;;
  ;; - `prettier-mode' to work
  ;;
  ;; - LSP to correctly identify the `ts-ls' language server for Typescript
  ;;   source files.
  ;;
  ;; Note that this requires an appropriate `use-package' invocation to load
  ;; the `add-node-modules-path' package.  Refer to ./use-package.el for
  ;; further infor.
  (add-node-modules-path)
  (init/common-programming-mode)

  ;; For some reason preemptively setting the fill column above is now not
  ;; taking hold in the following modes: `tide-mode', `js-mode'.
  ;;
  ;; Leave this statement at the end of this defun or it may not run.
  (setq-local fill-column init/defaults/fill-column)
  (run-with-idle-timer .1 nil #'(lambda ()
                                  (setq-local fill-column init/defaults/fill-column))))

;;; programming.el ends here
