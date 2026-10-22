;;; init-grep.el --- Configure grep  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defvar grep-use-null-device)

(defvar my/git-grep-find-command '("git --no-pager grep -nP '%SUBJECT%' -- :/" . 26)
  "Git grep command template and insertion position for the search text.")

(defun my/git-grep(command-args)
  "Run a grep search for QUERY using git.

The arguments specified by COMMAND-ARGS are passed to \"git grep\" without any
modification.  The user is prompted to customise the arguments passed to git if
the function is executed as a command.

The suffix \":/\" instructs git to search from the root of the project's
repository and should be removed if the search is to be conducted from the
current working directory."
  (interactive
   (progn
     (let* ((word (or (my/get-thing-at-point) ""))
            (command (replace-regexp-in-string
                       (regexp-quote "%SUBJECT%")
                       word
                       (car my/git-grep-find-command)
                       t 'literal)))
       (list (read-shell-command
              "Run git-grep (like this): "
              (cons command
                    (+ (length word) (cdr my/git-grep-find-command)))
              'grep-find-history)))))
  ;; Git grep already supplies file names; avoid adding /dev/null redirection.
  (let ((grep-use-null-device nil))
    (grep command-args)))

(use-package grep
  :ensure nil
  :defer t
  :custom
  ;; Don't ask to save buffers when running `grep-find'.
  (grep-save-buffers nil))

(provide 'init-grep)

;;; init-grep.el ends here
