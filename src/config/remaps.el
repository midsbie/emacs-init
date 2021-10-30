;;; remaps.el --- General keyboard remappings

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
;; Documentation can be found at the following locations:
;;
;;   * https://oremacs.com/swiper/
;;
;; Note that there advanced support for intricate actions in the mini-buffer
;; involving multiple selections and whatnot.  These are not fully understood
;; and it might be of interest to look into it at some point in time.

;;

;;; Code:

;; Awesome key bindings for moving around and manipulating windows.
(global-set-key [M-left]              'windmove-left)
(global-set-key [M-right]             'windmove-right)
(global-set-key [M-up]                'windmove-up)
(global-set-key [M-down]              'windmove-down)
(global-set-key (kbd "C-x p")         'other-previous-window)

;; Define miscellaneous shortcuts.
; Note that ivy defines some keyboard mappings under F2
; F2 F2 originally invoked: `2C-two-columns'
(global-set-key (kbd "<f2> <f2>")     'shell)
(global-set-key [f3]                  'better-next-error)
(global-set-key [S-f3]                'better-previous-error)
(global-set-key (kbd "C-x k")         'kill-current-buffer)
(global-set-key (kbd "C-x w")         'copy-sexp-to-kill-ring)

;; Buffer-related
(global-set-key (kbd "C-x x")         'mark-whole-buffer)
; Was previously using `truncate-lines` but now using the much more useful
; visual lines mode:
;  (global-set-key (kbd "C-S-w")         'toggle-truncate-lines)
(global-set-key (kbd "C-S-w")         'visual-line-mode)
(global-set-key (kbd "M-r")           'revert-buffer)
(global-set-key (kbd "C-x C-b")       'ibuffer)
; C-c g runs counsel-git-grep but we favour our own version since counsel was
; found to hang too frequently and cause memory leaks.
(global-set-key (kbd "C-c g")         'git-grep)
(global-set-key (kbd "C-c G")         'find-grep)
(global-set-key (kbd "C-x /")         'bury-buffer)
(global-set-key (kbd "C-x 4 k")       'kill-other-buffer)
(global-set-key (kbd "C-x 4 /")       'bury-other-buffer)
(global-set-key (kbd "C-x +")         'rearrange-desktop)
; Prevents collision with ivy's C-x l
(global-set-key (kbd "C-x L")         'count-lines-page)

(global-set-key (kbd "C-c C-t")       'c-toggle-hungry-state)

;; Redefine C-h (help) as C-x h and define backspace as C-h
(global-set-key (kbd "C-h")           'delete-backward-char)
(global-set-key (kbd "C-x h")         'help-command)

;; Replace mark-paragraph
(global-set-key (kbd "M-h")           'backward-kill-word)

;; Ensure that C-left is bound to M-b and C-right to M-f so that keys work
;; consistently when subword-mode is active.
(setf (global-key-binding (kbd "C-<left>")) (kbd "M-b")
      (global-key-binding (kbd "C-<right>")) (kbd "M-f"))

;;; remaps.el ends here