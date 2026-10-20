;;; my-elisp.el --- Emacs Lisp utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun my/execute-first-in-list (function-list)
  "Execute each function in FUNCTION-LIST in order, until one succeeds."
  (catch 'done
    (dolist (fn function-list)
      (condition-case nil
          (progn
            (funcall fn)
            (throw 'done nil))
        (error nil)))))

(provide 'my-elisp)

;;; my-elisp.el ends here
