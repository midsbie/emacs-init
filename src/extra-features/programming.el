;;; programming.el --- Assorted convenience features to improve programming
;;; experience

;; Copyright (C) 2020-2023  Miguel Guedes

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

(defun my/fix-indent-inverted-behaviour()
  "Fix RET and C-j inverted newline and indentation behaviour.
This function fixes the inverted indentation behaviour of RET and
C-j that occurs in major modes (e.g. web-mode).
"
  (local-set-key (kbd "C-j") 'newline-and-indent)
  (local-set-key (kbd "RET") 'newline))

(defun my/better-next-error ()
"Go to next error.
Attempts to jump to the next error as managed by `tide-mode' if
the active project's error buffer is not visible, otherwise
reverts to the default `next-error' defun."
  (interactive)
  (condition-case nil
      (or
       (let* ((win (get-buffer-window (tide-project-errors-buffer-name)))
             (buf (window-buffer win)))
         (when buf
           (with-current-buffer buf
             (set-window-point
              (get-buffer-window buf)
              (tide-find-next-error (point) 1))
             (tide-goto-error)
             t)))
       (next-error))
     (error
      (next-error))))

(defun my/better-previous-error ()
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

;;; programming.el ends here