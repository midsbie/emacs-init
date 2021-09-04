;;; csharp.el --- Configures `csharp-mode'

;; Copyright (C) 2021  Miguel Guedes

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

;;; Log:

;;; Code:

(defun init/mode/csharp ()
  "Initialise csharp mode."
  (lsp)
  (setq-local c-basic-offset 4)
  )

(use-package csharp-mode
  :mode (("\\.cs\\'" . csharp-mode))
  :init
  :hook (csharp-mode . init/mode/csharp)
  :config
  (add-hook 'before-save-hook 'lsp-format-buffer)
  )

;;; csharp.el ends here
