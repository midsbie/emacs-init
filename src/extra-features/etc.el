;;; etc.el --- Assorted extra features

;; Copyright (C) 2020  Miguel Guedes

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

(defun clear-before-save-hooks()
  "Clear all `before-save-hook' lists in all buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (progn
      (with-current-buffer buf
        (setq before-save-hook nil)))))

(defun fix-indent-inverted-behaviour()
  "Fix RET and C-j inverted newline and indentation behaviour.
This function fixes the inverted indentation behaviour of RET and
C-j that occurs in major modes (e.g. web-mode).
"
  (local-set-key (kbd "C-j") 'newline-and-indent)
  (local-set-key (kbd "RET") 'newline))

(defun get-word (word &optional default-string)
  "Return WORD if word, DEFAULT-STRING or \"\"."
  (or (and word (not (string-match "[\s]" word)) word)
      (or default-string "")))

(defun get-word-at-point ()
  "Return word at point or empty string if not word."
  (get-word (thing-at-point 'sexp 'no-properties)))

(defun get-kill-ring-word (&optional default-string)
  "Return last entry in kill ring if word or DEFAULT-STRING."
  (let ((top-entry (current-kill 0 t)))
    (get-word top-entry default-string)))

(defun copy-sexp-to-kill-ring ()
  "Copy sexp at point to kill ring."
  (interactive)
  (let ((sexp (thing-at-point 'sexp 'no-properties)))
    (when sexp
      (kill-new sexp)
      (message "copied: %s" sexp))))

(defun better-next-error ()
"Go to next error.
Attempts to jump to the next error as managed by `tide-mode',
otherwise reverts to the default `next-error' defun."
  (interactive)
  (condition-case nil
      (let ((buf (tide-project-errors-buffer-name)))
        (with-current-buffer buf
          (set-window-point
           (get-buffer-window buf)
           (tide-find-next-error (point) 1))
          (tide-goto-error)))
     (error
      (next-error))))

(defun better-previous-error ()
"Go to previous error.
Attempts to jump to the previous error as managed by `tide-mode',
otherwise reverts to the default `previous-error' defun."
  (interactive)
  (condition-case nil
      (let ((buf (tide-project-errors-buffer-name)))
        (with-current-buffer buf
          (set-window-point
           (get-buffer-window buf)
           (tide-find-previous-error (point) 1))
          (tide-goto-error)))
     (error
      (previous-error))))

;; The following meant to be exposed as a command; do not prefix the function name.
(defun google(query)
  "Open Google search with QUERY as parameter.
Prompts the user for a query string, if not provided, that is
constructed as a URL that causes the current browser provider to
run and navigate to the Google search page showing results for
the specified query string."
  (interactive "sQuery string: ")
  (browse-url (concat "https://google.com/search?q="
               (mapconcat 'identity (split-string query " ") "+"))))

;;; etc.el ends here