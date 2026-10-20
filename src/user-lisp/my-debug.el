;;; my-debug.el --- Debugging utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

;;;###autoload
(defun debug-message (format-string &rest args)
  "Print a debugging message to the *Messages* buffer.

Does not interfer with the minibuffer and keeps the special *Messages*
buffer scrolled to the bottom."
  (with-current-buffer (get-buffer-create "*Messages*")
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "%s\n" (apply 'format format-string args))))
    (let ((win (get-buffer-window (current-buffer))))
      (when win
        (set-window-point win (point-max))))))

;;;###autoload
(defun format-like-print (object)
  "Format an object into a string, similar to how `print` would output it."
  (cond
   ((stringp object)       ; Print strings with quotes
    (format "%S" object))
   ((listp object)         ; Recursively format list elements
    (format "'%s" (mapconcat #'format-like-print object " ")))
   ((vectorp object)       ; Convert vector to list and format
    (format "[ %s ]" (mapconcat #'format-like-print (append object nil) " ")))
   (t                       ; Numbers, symbols, and other types
    (format "%s" object))))

(provide 'my-debug)

;;; my-debug.el ends here
