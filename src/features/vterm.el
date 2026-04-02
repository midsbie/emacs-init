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

(defun init/vterm/enable ()
  "Enable vterm buffer settings.
Called from `vterm-mode-hook' to set up per-buffer defaults."
  (display-line-numbers-mode -1))

(defun init/vterm/config ()
  "Configure `vterm' package."
  (advice-add 'vterm-copy-mode :after #'init/vterm/copy-mode-cursor-fix))

(defun init/vterm/restore-cursor ()
  "Restore cursor settings based on `init/vterm/hollow-cursor-mode' state.
When the mode is active, set cursor to `hollow'; otherwise, remove the
buffer-local cursor overrides."
  (if init/vterm/hollow-cursor-mode
      (setq-local cursor-type 'hollow
                  blink-cursor-alist '((hollow . hollow)))
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'blink-cursor-alist)))

(define-minor-mode init/vterm/hollow-cursor-mode
  "Use a hollow cursor in the current vterm buffer.

Some TUI applications render their own cursor at point (e.g. Claude
Code) using terminal escape sequences (reverse video, custom colors,
etc.).  Emacs also draws its own cursor at point via the `cursor' face.
A filled `box' cursor covers the terminal-rendered cursor completely,
and depending on the color scheme the two can cancel each other out,
making the cursor invisible.

A `hollow' (outline-only) cursor avoids this: it does not fill the
character cell, so the TUI application's cursor shows through.  The
blink off-state is also set to `hollow' so that `blink-cursor-mode'
never hides the outline.

Toggle this mode off to restore the default cursor when no longer
running a TUI that conflicts."
  :lighter " Hollow"
  (init/vterm/restore-cursor))

(defun init/vterm/copy-mode-cursor-fix (&rest _)
  "Force a visible cursor in `vterm-copy-mode'.

Some TUI applications (e.g. Claude Code) set terminal colors that cause the
Emacs cursor to become invisible in copy mode.  This works around the issue by
temporarily remapping the `cursor' face to use the default foreground color and
switching to a filled `box' cursor for the duration of copy mode.

On the first entry into copy mode, `init/vterm/hollow-cursor-mode' is
automatically enabled so that subsequent normal-mode editing uses a hollow
cursor.  On exit, cursor settings are restored according to the mode's state."
  (if vterm-copy-mode
      (progn
        (unless init/vterm/hollow-cursor-mode
          (init/vterm/hollow-cursor-mode 1))
        (when (bound-and-true-p vterm--copy-mode-cursor-remap)
          (face-remap-remove-relative vterm--copy-mode-cursor-remap))
        (setq-local cursor-type 'box
                    blink-cursor-alist '((box . box))
                    vterm--copy-mode-cursor-remap
                    (face-remap-add-relative 'cursor :background (face-foreground 'default))))
    (when (bound-and-true-p vterm--copy-mode-cursor-remap)
      (face-remap-remove-relative vterm--copy-mode-cursor-remap)
      (kill-local-variable 'vterm--copy-mode-cursor-remap)
      (init/vterm/restore-cursor))))

(use-package vterm
  :hook (vterm-mode . init/vterm/enable)
  :bind (:map vterm-mode-map
         ([remap delete-backward-char] . vterm-send-backspace))
  :config (init/vterm/config))

;;; vterm.el ends here
