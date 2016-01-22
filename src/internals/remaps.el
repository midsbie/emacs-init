;;; remaps.el --- General keyboard remappings

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

;; Awesome key bindings for moving around and manipulating windows.
(global-set-key [M-left]              'windmove-left)
(global-set-key [M-right]             'windmove-right)
(global-set-key [M-up]                'windmove-up)
(global-set-key [M-down]              'windmove-down)
(global-set-key (kbd "C-x p")         'other-previous-window)

;; Define miscellaneous shortcuts.
(global-set-key [f2]                  'shell)
(global-set-key [f3]                  'next-error)
(global-set-key [S-f3]                'previous-error)
(global-set-key (kbd "C-x k")         'kill-this-buffer)

;; Buffer-related
(global-set-key (kbd "C-x x")         'mark-whole-buffer)
(global-set-key (kbd "C-S-w")         'toggle-truncate-lines)
(global-set-key (kbd "M-r")           'revert-buffer)
(global-set-key (kbd "C-x C-b")       'ibuffer)
(global-set-key (kbd "C-x g")         'find-grep)
(global-set-key (kbd "C-x /")         'bury-buffer)
(global-set-key (kbd "C-x 4 k")       'kill-other-buffer)
(global-set-key (kbd "C-x 4 /")       'bury-other-buffer)
(global-set-key (kbd "C-x +")         'rearrange-desktop)

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
