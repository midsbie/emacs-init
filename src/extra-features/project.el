;;; project.el --- Assorted functions for project file location and management

;; Copyright (C) 2025  Miguel Guedes

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

(defun my/project-copy-relative-file-path ()
  "Copy the current buffer's file path, relative to the current project root.
The path is pushed to the kill ring and also echoed with `message'."
  (interactive)
  (let* ((project (or (project-current)
                      (user-error "No project found")))
         (project-root (project-root project))
         (file (or (buffer-file-name (buffer-base-buffer))
                   (user-error "Current buffer is not visiting a file")))
         (relative (file-relative-name file project-root)))
    (kill-new relative)
    (message "%s" relative)))

;;; project.el ends here
