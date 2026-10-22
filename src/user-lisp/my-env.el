;;; my-env.el --- Shell environment utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

;; The value alternatives must match the rest of the line, so an unterminated
;; quote fails to parse instead of reading as an unquoted value.  The blanks
;; after = and the unquoted value are non-greedy so that a blank followed by #
;; starts a comment, even when the value is empty.
(defconst my/env--assignment-regexp
  (rx bos (* blank) (? "export" (+ blank))
      (group (any "A-Za-z_") (* (any "A-Za-z0-9_")))
      (* blank) "=" (*? blank)
      (or (seq ?\" (group (* (or (not (any ?\" ?\\)) (seq ?\\ anychar)))) ?\")
          (seq ?' (group (* (not (any ?')))) ?')
          (group (? (not (any ?\" ?' blank)) (*? nonl))))
      (? (+ blank) ?# (* nonl))
      (* blank) eos)
  "Regexp matching an assignment line of an environment file.
Groups 1 to 4 match the name and the double-quoted, single-quoted or
unquoted value.")

(defun my/env--read-file (filename)
  "Return the (NAME . VALUE) assignments in environment file FILENAME.
See `my/set-env-vars-from-file' for the syntax."
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((line-number 0) assignments)
      (dolist (line (split-string (buffer-string) "\n") (nreverse assignments))
        (setq line-number (1+ line-number))
        (cond
         ((string-match-p (rx bos (* blank) (? ?# (* nonl)) eos) line))
         ((string-match my/env--assignment-regexp line)
          (push (cons (match-string 1 line)
                      (cond ((match-beginning 2)
                             (replace-regexp-in-string
                              (rx ?\\ (group (any ?\" ?\\ ?n)))
                              (lambda (escape)
                                (let ((char (match-string 1 escape)))
                                  (if (equal char "n") "\n" char)))
                              (match-string 2 line) t t))
                            ((match-beginning 3) (match-string 3 line))
                            (t (match-string 4 line))))
                assignments))
         (t (user-error "%s:%d: Not an assignment: %s" filename line-number line)))))))

;;;###autoload
(defun my/set-env-vars-from-file (filename)
  "Set environment variables from the environment file FILENAME.

Each line is blank, a comment starting with #, or an assignment
NAME=VALUE, optionally preceded by \"export\".  NAME is a shell
identifier.  A VALUE in double quotes has \\\" for a double quote, \\\\
for a backslash and \\n for a newline, and any other backslash is
literal; one in single quotes is taken literally, as is an unquoted
VALUE stripped of surrounding blanks.  A # preceded by a blank starts
a comment that runs to the end of the line; any other # is part of the
VALUE.  Values are not expanded.  Any other line is an error, and then
no variable is set.

Setting PATH also puts its directories first in `exec-path', without
repeating ones already there."
  (interactive "FSelect .env file: ")
  (pcase-dolist (`(,name . ,value) (my/env--read-file filename))
    (setenv name value)
    (when (equal name "PATH")
      (let ((dirs (mapcar (lambda (dir) (and dir (directory-file-name dir)))
                          (parse-colon-path value))))
        (setq-default exec-path
                      (append dirs (seq-difference (default-value 'exec-path) dirs)))))))

(provide 'my-env)

;;; my-env.el ends here
