;;; grep.el --- grep command extensions

;; Copyright (C) 2019  Miguel Guedes

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

;;

;;; Code:

;; Settings
;; ----------------------------------------
; Don't ask to save buffers when running `grep-find'
(setq-default grep-save-buffers nil)

(setq git-grep-find-command '("git --no-pager grep -nP '' -- :/" . 26))

;; Functions
;; ----------------------------------------
(defun git-grep(command-args)
  "Run a grep search for QUERY using git.

The arguments specified by COMMAND-ARGS are passed to \"git grep\" without any
modification.  The user is prompted to customise the arguments passed to git if
the function is executed as a command.

The suffix \":/\" instructs git to search from the root of the project's
repository and should be removed if the search is to be conducted from the
current working directory."
  (interactive
   (progn
     (list (read-shell-command "Run git-grep (like this): "
                               git-grep-find-command 'grep-find-history))))
    (let* ((last-grep-use-null-device grep-use-null-device))
      ; We must set `grep-use-null-device' to nil or we get a strange error
      ; involving output redirection to /dev/null
      (setq grep-use-null-device nil)
      (grep command-args)
      ; Restore variable state
      (setq grep-use-null-device last-grep-use-null-device)))

;; Deprecated functions
;; ----------------------------------------
;; The following function is kept for the purpose of future reference, in
;; particular as an illustration of how to use the universal argument.
(defun git-grep-OLD (query)
  "Run a grep search for QUERY using git.

When executed without the universal \
argument (\\[universal-argument]), runs \ \"git grep\" from the \
repository's root directory.  When the universal argument is \
specified but no number, \"git grep\" is executed from the current working \
directory.  If \"1\" is specified as the universal argument, a regex \
search is conducted.  Otherwise, a case insensitive search is run."
  (interactive "sgit grep: ")

  (let ((end "") (args ""))
    ; This gem about the presence of the universal argument was derived from
    ; the answer at https://stackoverflow.com/a/56853097
    (cond ((eq current-prefix-arg 1) (setq args "-e"))
          ((eq current-prefix-arg nil)
           (setq args "-i"
                 end " -- :/")))

    (let* ((last-grep-use-null-device grep-use-null-device))
      ; We must set `grep-use-null-device' to nil or we get a strange error
      ; involving output redirection to /dev/null
      (setq grep-use-null-device nil)
      (grep (concat "git --no-pager grep -n " args " \"" query "\"" end))
      ; Restore variable state
      (setq grep-use-null-device last-grep-use-null-device)))
  )

;;; grep.el ends here
