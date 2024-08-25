;;; buffer.el --- Extra features for buffer manipulation

;; Copyright (C) 2023  Miguel Guedes

;; Author: Miguel Guedes <miguel@softgeist.com>
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

(defun my/beginning-of-statement ()
  "Safe version of `c-beginning-of-statement-1'.

Attempts to run the original function.  In web-related modes like
`typescript-ts-mode' errors may occur, in which case it will
attempt to provide a similar result using alternative methods."
  (interactive)
  (my/execute-first-in-list init/beginning-of-statement-fn))

(defun my/end-of-statement ()
  "Safe version of `c-end-of-statement'.

Attempts to run the original function.  In web-related modes like
`typescript-ts-mode' errors may occur, in which case it will
attempt to provide a similar result using alternative methods."
  (interactive)
  (my/execute-first-in-list init/end-of-statement-fn))

(defun my/select-current-word ()
  "Select the word under cursor."
  (interactive)
  (let ((subword-enabled (and (fboundp 'subword-mode) subword-mode)))
    (when subword-enabled
      (subword-mode -1))
    (unless (looking-at "\\<")
      (backward-word))
    (set-mark-command nil)
    (forward-word)
    (when subword-enabled
      (subword-mode 1))))

(defun my/kill-buffers-with-nonexistent-files ()
  "Kill all buffers associated with files that no longer exist."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((file (buffer-file-name buffer)))
      (when (and file (not (file-exists-p file)))
        (kill-buffer buffer)))))

(defun my/untabify-buffer ()
  "Convert tabs to spaces in the current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun my/get-word (word &optional default-string)
  "Return WORD if word, DEFAULT-STRING or \"\"."
  (or (and word (not (string-match "[\s]" word)) word)
      (or default-string "")))

(defun my/get-word-at-point ()
  "Return word at point or empty string if not word."
  (my/get-word (thing-at-point 'sexp 'no-properties)))

(defun my/get-kill-ring-word (&optional default-string)
  "Return last entry in kill ring if word or DEFAULT-STRING."
  (let ((top-entry (current-kill 0 t)))
    (my/get-word top-entry default-string)))

(defun my/copy-symbol-to-kill-ring ()
  "Copy symbol at point to kill ring."
  (interactive)
  (let ((symbol (symbol-name (symbol-at-point))))
    (when symbol
      (kill-new symbol)
      (message "copied: %s" symbol))))

(defun my/kill-other-buffer (count &optional all-frames)
  "Select next window and kill buffer associated with it.
COUNT specified the number of windows to skip, starting with the
selected window, before making the selection.  If COUNT is
positive, skip COUNT forwards.  If COUNT is negative, skip COUNT
backwards.  In an interactive call, COUNT is the numeric prefix
argument.  When non-nill, ALL-FRAMES specifies whether to take
into account buffers in all frames."
  (interactive "p")
  (save-selected-window
    (other-window (* count 1) all-frames)
    (kill-buffer)))

(defun my/bury-other-buffer (count &optional all-frames)
  "Select next window and bury buffer associated with it.
COUNT specified the number of windows to skip, starting with the
selected window, before making the selection.  If COUNT is
positive, skip COUNT forwards.  If COUNT is negative, skip COUNT
backwards.  In an interactive call, COUNT is the numeric prefix
argument.  When non-nill, ALL-FRAMES specifies whether to take
into account buffers in all frames."
  (interactive "p")
  (save-selected-window
    (other-window (* count 1) all-frames)
    (bury-buffer)))

;;; buffer.el ends here
