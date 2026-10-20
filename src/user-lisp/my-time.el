;;; my-time.el --- Time utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

;; Taken verbatim from: http://www.emacswiki.org/emacs/DotEmacsChallenge
;;;###autoload
(defun my/date (&optional insert)
  "Display the current date and time.
With a prefix arg, INSERT it into the buffer."
  (interactive "P")
  (funcall (if insert 'insert 'message)
           (format-time-string "%a, %d %b %Y %T %Z" (current-time))))

(provide 'my-time)

;;; my-time.el ends here
