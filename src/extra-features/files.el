;;; files.el --- Assorted filesystem functions

;; Copyright (C) 2021-2023  Miguel Guedes

;; Author: Miguel Guedes <miguel@miguelguedes.org>
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

(defun my/locate-file-in-dominating-node-modules (file from-path)
  "Attempt to locate FILE inside a dominating node_modules
directory from FROM-PATH."
  (my/locate-file-recursive (concat "node_modules/" file) from-path))

(defun my/locate-file-recursive (file path)
  "Locate FILE recursively from PATH."
  (let (lpath)
    (cl-pushnew path lpath)
    (or (locate-file file lpath nil)
;;                     (lambda (f) (and (file-directory-p f) 'dir-ok)))
        (unless (string= "/" path)
          (my/locate-file-recursive file
                                    (file-name-directory (directory-file-name path)))))))

(defun my/dir-is-parent-p (dir path)
  "Return t if DIR is parent of PATH."
  (let* ((parent (directory-file-name (file-name-directory (directory-file-name path))))
         (basename (file-name-nondirectory parent)))
    (unless (string= "/" path)
      (or (string= basename dir)
          (my/dir-is-parent-p dir parent)))))

(defun my/maybe-load-library (name)
  "Attempt to load library NAME.
Produces a message if it was not possible to load the library and
does not interrupt execution."
  (condition-case err
      (load-library (name))
    (error (princ (format "Failed to load library: %s (reason: %s)" name err)))))

(defun my/read-lines (file)
  "Return a list of lines of a file given by FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

;;; files.el ends here
