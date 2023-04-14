;;; vala.el --- Configures `vala-mode'

;; Copyright (C) 2023  Miguel Guedes

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
    ;; Ensure that the idiomatic "vala" style is used in `vala-mode'.
    (unless (assoc 'vala-mode c-default-style)
      (add-to-list 'c-default-style '(vala-mode . "vala"))))

(defun init/vala/config ()
  "Configure `vala-mode'."

  (init/common-nonweb-programming-mode)
  (c-toggle-auto-hungry-state 1)

  ;; This hook must be set up BEFORE the LSP buffer formatting one below to
  ;; ensure it runs LAST, otherwise LSP will replace spaces for tabs.
  (add-hook 'before-save-hook 'untabify-buffer nil t)

  (when (executable-find "uncrustify")
    (add-hook 'before-save-hook 'lsp-format-buffer nil t)))

(use-package vala-mode
  :hook ((vala-mode . init/vala/config))
  :config (init/vala))

;;; c++.el ends here
