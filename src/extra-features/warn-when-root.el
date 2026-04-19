;;; warn-when-root.el --- Change modeline background when acting as root user

;; Copyright (C) 2015-2026  Miguel Guedes

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


(defun my/warn-when-root-visits-file ()
  "Warn when visiting a buffer whose default directory belongs to root.
Displays the mode line with a red background when the effective
user is root via TRAMP (sudo, su, doas, or multi-hop)."
  (when (and (not (bound-and-true-p my/--root-warning-applied))
             (file-remote-p default-directory)
             (string= "root" (file-remote-p default-directory 'user)))
    (face-remap-add-relative
     'mode-line-active
     '(:background "red3" :foreground "white" :box nil))
    (face-remap-add-relative
     'mode-line-inactive
     '(:background "#4a0000" :foreground "gray40" :box nil))
    (setq-local my/--root-warning-applied t)))

;; Change mode line background colour if file being edited as root.
(add-hook 'after-change-major-mode-hook 'my/warn-when-root-visits-file)

;;; warn-when-root.el ends here
