;;; vterm.el --- Configures the vterm feature

;; Copyright (C) 2026  Miguel Guedes

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

;; Configuration for vterm terminal emulator.

;;; Code:

(defun init/vterm/config ()
  "Configure `vterm' package."
  (advice-add 'vterm-copy-mode :after #'init/vterm/copy-mode-cursor-fix)
  (define-key vterm-mode-map [remap delete-backward-char] #'vterm-send-backspace))

(defun init/vterm/copy-mode-cursor-fix (&rest _)
  "Force a visible cursor in `vterm-copy-mode'.
Some TUI applications (e.g. Claude Code) set terminal colors that cause the
Emacs cursor to become invisible in copy mode.  This works around the issue by
remapping the `cursor' face and suppressing cursor blink."
  (if vterm-copy-mode
      (progn
        (when (bound-and-true-p vterm--copy-mode-cursor-remap)
          (face-remap-remove-relative vterm--copy-mode-cursor-remap))
        (setq-local cursor-type 'box
                    blink-cursor-alist '((box . box))
                    vterm--copy-mode-cursor-remap
                    (face-remap-add-relative 'cursor :background (face-foreground 'default))))
    (when (bound-and-true-p vterm--copy-mode-cursor-remap)
      (face-remap-remove-relative vterm--copy-mode-cursor-remap)
      (kill-local-variable 'vterm--copy-mode-cursor-remap)
      (kill-local-variable 'blink-cursor-alist))))

(use-package vterm
  :config (init/vterm/config))

;;; vterm.el ends here
