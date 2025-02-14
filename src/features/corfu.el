;;; corfu.el --- Customises the Corfu package

;; Copyright (C) 2024-2025  Miguel Guedes

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

;; Documentation at: https://github.com/minad/corfu
(use-package corfu
  :init
  (global-corfu-mode)

  :custom
  (corfu-auto t)                 ; Enable auto completion
  (corfu-auto-prefix 1)
  (corfu-separator ?\s)          ; Orderless field separator
  (corfu-quit-at-boundary 'separator)
  ;; (corfu-cycle t)                ; Enable cycling for `corfu-next/previous'
  (corfu-quit-no-match nil)         ; Quit when there is no match
  ;; (corfu-preview-current nil)    ; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ; Use scroll margin
  :bind (:map corfu-map
              ("S-SPC" . corfu-insert-separator)
              ("M-h" . backward-kill-word)))

;; Configuration of Emacs' completion-at-point settings
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

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

  :bind ("M-RET" . completion-at-point))

;;; corfu.el ends here
