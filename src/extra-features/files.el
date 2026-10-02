;;; files.el --- Assorted filesystem functions

;; Copyright (C) 2021-2026  Miguel Guedes

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

(defun my/find-dired-backup (dir)
  "Run `find-dired' in DIR for Emacs backup and auto-save files.
Matches auto-save files (`#NAME#'), simple backups (`NAME~') and
version-numbered backups (`NAME.~N~').
DIR defaults to the current project root when available, otherwise
to `default-directory'."
  (interactive
   (let* ((proj (project-current))
          (default (if proj (project-root proj) default-directory)))
     (list (read-directory-name "Find backups in directory: " default))))
  (find-dired dir "\\( -name '#*#' -o -name '*~' \\)"))


;;; Re-visiting files with or without root privileges
;;
;; `my/find-file-as-root' re-opens the current buffer through Tramp's `/sudo::'
;; method (delegating to the native `tramp-revert-buffer-with-sudo');
;; `my/find-file-as-user' performs the reverse, stripping a sudo/su hop; and
;; `my/toggle-file-root' flips between the two based on the current path.  All
;; three operate on file-visiting and Dired buffers.

(defvar my/root-tramp-methods '("su" "sudo" "doas" "ksu" "sg" "sudoedit")
  "Tramp methods that denote elevated (root) access.
Used by `my/file-name-elevated-p' to decide whether a buffer is already
visited with raised privileges.")

(defun my/file-name-elevated-p (filename)
  "Return non-nil if FILENAME is a Tramp file reached via an elevation method.
Only the final hop is considered, so a remote file tunnelled through
sudo (e.g. \"/ssh:host|sudo:host:/etc\") counts as elevated."
  (and (stringp filename)
       (tramp-tramp-file-p filename)
       (member (tramp-file-name-method (tramp-dissect-file-name filename))
               my/root-tramp-methods)))

(defun my/file-name-without-sudo (filename)
  "Return FILENAME with a trailing elevation hop (sudo, su, ...) removed.
If FILENAME is reached through a multi-hop chain, only the final
elevation hop is stripped and the preceding chain is preserved.  When
FILENAME is not visited via an elevation method it is returned
unchanged."
  (if (not (my/file-name-elevated-p filename))
      filename
    (let* ((v (tramp-dissect-file-name filename))
           (hop (tramp-file-name-hop v))
           (localname (tramp-file-name-localname v)))
      (if (and hop (string-suffix-p tramp-postfix-hop-format hop))
          ;; Drop the final (elevation) hop, keep the chain leading to it.
          (concat tramp-prefix-format
                  (substring hop 0 (- (length tramp-postfix-hop-format)))
                  tramp-postfix-host-format
                  localname)
        ;; The elevation was the sole hop: fall back to the plain file.
        localname))))

(defun my/dired-directory-without-sudo (dired-dir)
  "Strip an elevation hop from DIRED-DIR, a `dired-directory' value.
DIRED-DIR may be a directory string or a list whose `car' is the
directory and whose `cdr' is an explicit file list."
  (cond ((stringp dired-dir) (my/file-name-without-sudo dired-dir))
        ((consp dired-dir)
         (cons (my/file-name-without-sudo (car dired-dir)) (cdr dired-dir)))
        (t dired-dir)))

(defun my/find-file-as-root ()
  "Re-open the file or directory in the current buffer with root privileges.
Delegates to `tramp-revert-buffer-with-sudo', re-visiting the buffer
through Tramp's `/sudo::' method.  Works for both file-visiting and
Dired buffers.  See also `my/find-file-as-user' and `my/toggle-file-root'."
  (interactive)
  (require 'tramp-cmds)
  (unless (or (buffer-file-name) (derived-mode-p 'dired-mode))
    (user-error "Buffer is not visiting a file or directory"))
  (tramp-revert-buffer-with-sudo))

(defun my/find-file-as-user ()
  "Re-open the file or directory in the current buffer as the normal user.
Removes a trailing sudo/su elevation hop from the current file name (or
Dired directory) and re-visits it without raised privileges.  For
multi-hop remote paths only the final elevation hop is removed.  See
also `my/find-file-as-root' and `my/toggle-file-root'."
  (interactive)
  (require 'tramp)
  (cond
   ((buffer-file-name)
    (let ((target (my/file-name-without-sudo (buffer-file-name))))
      (when (string-equal target (buffer-file-name))
        (user-error "Buffer is not visited with elevated privileges"))
      (find-alternate-file target)))
   ((derived-mode-p 'dired-mode)
    (let ((old (expand-file-name default-directory))
          (new (my/file-name-without-sudo default-directory)))
      (when (string-equal new default-directory)
        (user-error "Directory is not visited with elevated privileges"))
      (dired-unadvertise old)
      (setq default-directory new
            list-buffers-directory (and list-buffers-directory
                                        (my/file-name-without-sudo
                                         list-buffers-directory))
            dired-directory (my/dired-directory-without-sudo dired-directory))
      (dired-advertise)
      (revert-buffer)))
   (t (user-error "Buffer is not visiting a file or directory"))))

(defun my/toggle-file-root ()
  "Toggle root privileges for the file or directory in the current buffer.
When the buffer is currently visited through an elevation method (sudo,
su, ...) re-open it as the normal user; otherwise re-open it with root
privileges.  Dispatches to `my/find-file-as-user' or
`my/find-file-as-root' accordingly."
  (interactive)
  (require 'tramp)
  (let ((name (or (buffer-file-name)
                  (and (derived-mode-p 'dired-mode)
                       (expand-file-name default-directory)))))
    (cond
     ((null name) (user-error "Buffer is not visiting a file or directory"))
     ((my/file-name-elevated-p name) (my/find-file-as-user))
     (t (my/find-file-as-root)))))

;;; files.el ends here
