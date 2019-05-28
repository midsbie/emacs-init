;;; libcommon --- Collection of useful defuns

;; Copyright (C) 2014-2019 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; URL:
;; Keywords: tools
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Used by init.el.

;;; Code:

;; Taken verbatim from: http://www.emacswiki.org/emacs/DotEmacsModular
(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY without the extension."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat (file-name-as-directory directory) path))
           (isdir (car (cdr element)))
           (ignore (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))

(defun read-lines (file)
  "Return a list of lines of a file given by FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

;; Taken verbatim from: http://www.emacswiki.org/emacs/DotEmacsChallenge
(defun date (&optional insert)
  "Display the current date and time.
With a prefix arg, INSERT it into the buffer."
  (interactive "P")
  (funcall (if insert 'insert 'message)
           (format-time-string "%a, %d %b %Y %T %Z" (current-time))))

(defun run-or-load (func library)
  "Execute FUNC or load LIBRARY if FUNC not defined.
Checks that the defun FUNC has been loaded before invoking it.  If it
hasn't been loaded, LIBRARY is loaded via a call to `load-library'."
  (message "checking: %s" (or (and (fboundp func) "YES") "NO") )
  (unless (fboundp func)
    (load-library (symbol-name library)))
  (call-interactively func))

(defmacro t-run-or-load (func library)
  "Return a lambda that can be attached to a key handler or similar handlers.
See documentation of `run-or-load' for a description of the FUNC
and LIBRARY variables."
  `(lambda()
     (interactive)
     (run-or-load ,func ,library)))

(defun kill-current-buffer ()
  "Simply deletes the currently active buffer.  Overcomes the
issues that `kill-this-buffer' is sometimes afflicted by where it
stops working.  This [1] post over on Reddit explains what
happens.

[1] https://www.reddit.com/r/emacs/comments/64xb3q/killthisbuffer_sometimes_just_stops_working/"
  (interactive)
  (kill-buffer (current-buffer)))


(provide 'libcommon)

;;; libcommon.el ends here
