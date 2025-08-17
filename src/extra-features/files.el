;;; files.el --- Assorted filesystem functions

;; Copyright (C) 2021-2025  Miguel Guedes

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

(defun my/locate-file-in-dominating-directory (file dir-name from-path)
  "Attempt to locate FILE inside a dominating DIR-NAME directory from FROM-PATH."
  (let ((dominating-dir (locate-dominating-file from-path dir-name)))
    (when dominating-dir
      (let ((file-path (expand-file-name (concat dir-name "/" file) dominating-dir)))
        (when (file-exists-p file-path)
          file-path)))))

(defun my/locate-file-in-dominating-node-modules (file from-path)
  "Return absolute path to FILE inside any ancestor node_modules, starting at FROM-PATH.
Walks up the directory tree; at each ancestor, if a node_modules exists,
checks for FILE inside it. Returns the first match or nil."
  (let* ((start (expand-file-name from-path))
         ;; If FROM-PATH is a file, search from its directory.
         (dir (if (file-directory-p start)
                  start
                (file-name-directory start)))
         (found nil))
    (while (and dir (not found))
      (let* ((nm (expand-file-name "node_modules" dir))
             (candidate (expand-file-name file nm)))
        (when (and (file-directory-p nm)
                   (file-exists-p candidate))
          (setq found candidate)))
      ;; move to parent; stop at filesystem root
      (let ((parent (file-name-directory (directory-file-name dir))))
        (setq dir (unless (or (null parent)
                              (string= parent dir))
                    parent))))
    found))

(defun my/locate-topmost-file (from-path file)
  "Starting at FROM-PATH, look up directory hierarchy for the topmost directory
containing FILE."
  (let ((last nil))
    (while from-path
      (let ((file-dir (locate-dominating-file from-path file)))
        (if file-dir
            (progn
              (setq last file-dir)
              (setq from-path (my/parent-directory (file-name-directory file-dir))))
          (setq from-path nil))))
    last))

(defun my/dir-is-parent-p (dir path)
  "Return t if DIR is parent of PATH."
  (let ((found nil)
        (cur path))
    (while (and (not found) cur)
      (setq found (string= (file-name-nondirectory (directory-file-name cur)) dir))
      (setq cur (my/parent-directory cur)))
    found))

(defun my/parent-directory (path)
  "Return the parent directory of PATH, or nil if it is the root."
  (let ((parent (file-name-directory (directory-file-name path))))
    (unless (equal parent path)
      parent)))

(defun my/maybe-load-library (name)
  "Attempt to load library NAME.
Produces a message if it was not possible to load the library and
does not interrupt execution."
  (condition-case err
      (load-library (name))
    (error (princ (format "Failed to load library: %s (reason: %s)" name err)))))

(defun my/read-file-lines (file)
  "Return a list of lines of a file given by FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

;;; files.el ends here
