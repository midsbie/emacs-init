;;; my-large-files.el --- Support for editing of large files  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2025  Miguel Guedes

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

;;;###autoload
(defun my/handle-large-file-hook (&rest _args)
  "Use read-only Fundamental mode for files over 1,048,576 characters.
Return non-nil when handled, for use as before-until advice on
`set-auto-mode'.  Ignore that function's arguments."
  (when (and buffer-file-name (> (buffer-size) (* 1024 1024)))
    (fundamental-mode)
    (setq buffer-read-only t)
    (buffer-disable-undo)
    t))

(provide 'my-large-files)

;;; my-large-files.el ends here
