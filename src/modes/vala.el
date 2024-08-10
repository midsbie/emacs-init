;;; vala.el --- Configures `vala-mode'

;; Copyright (C) 2023-2024  Miguel Guedes

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

(defun init/vala ()
  "Initialise `vala-mode'."
  ;; Add this mode to LSP's formatting indent alist to ensure that the buffer is
  ;; formatted correctly.
  (push '(vala-mode . c-basic-offset) lsp--formatting-indent-alist)

  ;; Ensure that the idiomatic "vala" style is used in `vala-mode'.
  (unless (assoc 'vala-mode c-default-style)
    (add-to-list 'c-default-style '(vala-mode . "vala"))))

(defun init/vala/config ()
  "Configure `vala-mode'."

  (init/common-nonweb-programming-mode)
  (c-toggle-auto-hungry-state -1)

  ;; Make sure to disable tabs mode to ensure buffer is formatted with spaces.
  (setq-local indent-tabs-mode nil))

(use-package vala-mode
  :hook ((vala-mode . init/vala/config))
  :config (init/vala))

;;; c++.el ends here
