;;; my-buffer.el --- Buffer editing utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'my-elisp)

(declare-function visual-fill-column-mode "ext:visual-fill-column")

;; Configuration loads after this file is compiled.
(defvar init/beginning-of-statement-fn)
(defvar init/end-of-statement-fn)

;;;###autoload
(defun my/beginning-of-statement ()
  "Safe version of `c-beginning-of-statement-1'.

Attempts to run the original function.  In web-related modes like
`typescript-ts-mode' errors may occur, in which case it will
attempt to provide a similar result using alternative methods."
  (interactive)
  (my/execute-first-in-list init/beginning-of-statement-fn))

;;;###autoload
(defun my/end-of-statement ()
  "Safe version of `c-end-of-statement'.

Attempts to run the original function.  In web-related modes like
`typescript-ts-mode' errors may occur, in which case it will
attempt to provide a similar result using alternative methods."
  (interactive)
  (my/execute-first-in-list init/end-of-statement-fn))

(defun my/get-thing-type-at-point (&optional arg)
  "Return the type of thing at point."
  (cond ((eql arg 0) 'word)
        (arg 'symbol)
        ((derived-mode-p 'prog-mode) 'symbol)
        (t 'word)))

;;;###autoload
(defun my/get-thing-at-point (&optional arg)
  "Return the symbol or word at point.
Without a prefix argument, return the symbol in programming modes and
the word elsewhere.  With a zero prefix ARG, return the word; with
\\[universal-argument], return the symbol.

A word spans a whole camelCase identifier even when `subword-mode'
is on.  A symbol also spans whatever the major mode's syntax table
joins to it, such as `foo_bar' in C or `my/foo' in Lisp."
  (if (eq (my/get-thing-type-at-point arg) 'symbol)
      (thing-at-point 'symbol t)
    ;; `subword-mode' installs a table that makes `forward-word'
    ;; stop inside camelCase; an empty one restores plain words.
    (let ((find-word-boundary-function-table
           (make-char-table nil)))
      (thing-at-point 'word t))))

;;;###autoload
(defun my/copy-thing-at-point (&optional arg)
  "Copy the symbol or word at point to the kill ring and clipboard.
Without a prefix argument, copy the symbol in programming modes and
the word elsewhere.  With a zero prefix ARG, copy the word; with
\\[universal-argument], copy the symbol.

A word spans a whole camelCase identifier even when `subword-mode'
is on.  A symbol also spans whatever the major mode's syntax table
joins to it, such as `foo_bar' in C or `my/foo' in Lisp."
  (interactive "P")
  (let ((text (my/get-thing-at-point arg)))
    (unless text
      (user-error "No %s at point" (my/get-thing-type-at-point arg)))
    (kill-new text)
    (message "Copied %s" text)))

;;;###autoload
(defun my/kill-buffers-with-nonexistent-files ()
  "Kill all buffers associated with files that no longer exist."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((file (buffer-file-name buffer)))
      (when (and file (not (file-exists-p file)))
        (kill-buffer buffer)))))

;;;###autoload
(defun my/untabify-buffer ()
  "Convert tabs to spaces in the current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

;;;###autoload
(defun my/cycle-line-wrapping ()
  "Cycle line wrapping through `fill-column', window width, and off.
Wrapping at `fill-column' enables both `visual-line-mode' and
`visual-fill-column-mode'.  The off state truncates long lines."
  (interactive)
  (cond
   ((bound-and-true-p visual-fill-column-mode)
    (visual-fill-column-mode -1)
    (visual-line-mode 1)
    (message "Line wrapping: window width"))
   (visual-line-mode
    (visual-line-mode -1)
    ;; Disabling `visual-line-mode' restores `truncate-lines', possibly to
    ;; nil. Force truncation for the off state. Re-enabling the mode sets
    ;; it to nil again; the fill-column state keeps that mode enabled.
    (setq-local truncate-lines t)
    (message "Line wrapping: off"))
   (t
    (visual-line-mode 1)
    (and (not (eq major-mode 'magit-status-mode))
         (visual-fill-column-mode 1))
    (message "Line wrapping: fill-column (%d)" fill-column))))

;;;###autoload
(defun my/unfill-paragraph (&optional region)
  "Inverse of `fill-paragraph'.
Join lines in the current paragraph into a single long line.
With active region, unfill all paragraphs in the region."
  (interactive (list t))
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil region)))

(defun my/get-word (word &optional default-string)
  "Return WORD if word, DEFAULT-STRING or \"\"."
  (or (and word (not (string-match "[\s]" word)) word)
      (or default-string "")))

(defun my/get-word-at-point ()
  "Return word at point or empty string if not word."
  (my/get-word (thing-at-point 'sexp 'no-properties)))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun my/insert-and-indent (count)
  "Insert the typed character COUNT times, then indent the current line."
  (interactive "p")
  (self-insert-command count)
  (indent-for-tab-command))

(provide 'my-buffer)

;;; my-buffer.el ends here
