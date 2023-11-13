;;; emacs.el --- Utilitarian functions related to Emacs itself

;; Copyright (C) 2021-2023  Miguel Guedes

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

;; Defun invoked after pressing C-x C-k (see `init/elisp').
;; Evals the current buffer and displays a message.
(defun my/eval-buffer ()
  "Evaluate the current buffer.
This command should only be used with ELISP."
  (interactive)
  (cl-block inner
      (cond
       ((or (eq major-mode 'emacs-lisp-mode)
            (eq major-mode 'lisp-interaction-mode))
        (eval-buffer))
       (t (message "unsupported mode: %s" major-mode)
          (cl-return-from inner)))

    (message "buffer evaluated")))

(defun my/clear-before-save-hooks()
  "Clear all `before-save-hook' lists in all buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (progn
      (with-current-buffer buf
        (setq before-save-hook nil)))))

;;; emacs.el ends here
