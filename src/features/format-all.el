;;; format-all.el --- Configures `format-all'

;; Copyright (C) 2024-2026  Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: internal, tools

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

(require 'project)
(require 'subr-x)

(defun init/format-all--clang-format-in-project-p ()
  (when-let* ((proj   (project-current nil default-directory))
              (root   (file-name-as-directory (file-truename (project-root proj))))
              (cfgdir (locate-dominating-file default-directory ".clang-format"))
              (cfgdir (file-name-as-directory (file-truename cfgdir))))
    (file-in-directory-p cfgdir root)))

(defun init/format-all/maybe-enable (lang)
  (if (init/format-all--clang-format-in-project-p)
      (progn
        (setq-local format-all-formatters `((,lang clang-format)))
        (format-all-mode 1))
    (format-all-mode -1)))

(defun init/format-all/csharp-maybe-enable ()
  (init/format-all/maybe-enable "C#"))

(use-package format-all
  :hook (csharp-mode . init/format-all/csharp-maybe-enable))

;;; format-all.el ends here
