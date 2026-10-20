;;; init-visual-fill-column.el --- Configure visual-fill-column  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wrap at each buffer's `fill-column' in `markdown-mode' and `org-mode'
;; buffers, and elsewhere when selected with C-S-w.  Plain `visual-line-mode'
;; continues to wrap at the window width.
;;
;; `visual-fill-column-mode' sizes the text area to `fill-column' columns, but
;; line numbers are drawn inside the text area, so text would wrap early by
;; the width of the line-number gutter.  The package leaves this to its users
;; through `visual-fill-column-extra-text-width', which widens the text area;
;; `init/visual-fill-column/enable' sets it to the gutter's width whenever
;; either mode is toggled.
;;
;; That width can only be known up front because init-display-line-numbers.el
;; sets `display-line-numbers-width-start': by default the gutter grows as the
;; window scrolls to lines with more digits, and nothing would recalculate the
;; margins.  A buffer that gains a digit in its line count while open still
;; widens its gutter, and wraps one column early until
;; `display-line-numbers-mode' is toggled.

;;; Code:

(defun init/visual-fill-column/enable ()
  "Widen the text area by the width of the line-number gutter.
Run from the hooks of both `visual-fill-column-mode' and
`display-line-numbers-mode', so that the margins follow the gutter
whichever mode is enabled last."
  (when visual-fill-column-mode
    (setq-local visual-fill-column-extra-text-width
                (cons 0 (if (and display-line-numbers
                                 (numberp display-line-numbers-width))
                            ;; The number is padded by a space on each side.
                            (+ display-line-numbers-width 2)
                          0)))
    ;; `visual-fill-column-adjust' acts on the selected window, which need
    ;; not show this buffer (e.g. when desktop restores it).
    (dolist (window (get-buffer-window-list nil nil t))
      (with-selected-window window
        (visual-fill-column-adjust)))))

(use-package visual-fill-column
  :demand
  :hook (((visual-fill-column-mode display-line-numbers-mode)
          . init/visual-fill-column/enable)
         ((markdown-mode org-mode) . visual-fill-column-mode))
  :custom
  (visual-fill-column-width nil)
  :config
  ;; Remove the previous coupling when this configuration is reloaded.
  (remove-hook 'visual-line-mode-hook #'visual-fill-column-for-vline)
  ;; Recalculate the margins after changing the column or zooming the text.
  (advice-add 'set-fill-column :after #'visual-fill-column-adjust)
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

(provide 'init-visual-fill-column)

;;; init-visual-fill-column.el ends here
