;;; init-corfu.el --- Configure corfu  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

;; Documentation at: https://github.com/minad/corfu
(use-package corfu
  :init
  (global-corfu-mode)

  :custom
  (corfu-auto t)                 ; Enable auto completion
  (corfu-auto-prefix 1)
  (corfu-separator ?\s)          ; Orderless field separator
  (corfu-quit-at-boundary 'separator)

  ;; Quit when there is no match, otherwise "No match" is printed and double RET
  ;; is required.
  (corfu-quit-no-match t)

  ;; Configure `global-corfu-mode' to only enable `corfu-mode' per the following
  ;; settings:
  (global-corfu-modes '((not shell-mode eshell-mode term-mode vterm-mode) t))
  (global-corfu-minibuffer nil)

  ;; (corfu-preview-current nil)    ; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ; Use scroll margin

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)

  :bind (("M-RET" . completion-at-point)
         :map corfu-map
         ("S-SPC" . corfu-insert-separator)
         ("M-h" . backward-kill-word)))

(provide 'init-corfu)

;;; init-corfu.el ends here
