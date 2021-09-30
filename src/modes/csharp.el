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

  ;; Don't set this to a value less than `lsp-idle-delay' above to prevent
  ;; unnecessary messages being sent to the server and potentially overloading
  ;; it.  Currently enforcing a minimum of 1s.
  (setq-local flycheck-idle-change-delay (if (< lsp-idle-delay 1) 1 lsp-idle-delay))
  ;; Flycheck's debouncer may no longer be necessary but here for now while it
  ;; is evaluated.
  (setq-local my/flycheck-buffer-time-between 1)
  ;; Explicitly forcing to default syncronization method of `nil`, which
  ;; defaults to `lsp--sync-full', as supported by omnisharp-roslyn.  This
  ;; statement is here to document the fact that lsp supports the
  ;; `lsp--sync-incremental', which may turn out to be more performant for some
  ;; specific projects.
  (setq-local lsp-document-sync-method nil)

  ;; Enable the following if flycheck should only check the buffer on load and
  ;; save.
  ;; (setq-local flycheck-check-syntax-automatically '(mode-enabled save))
  )

(use-package csharp-mode
  :mode (("\\.cs\\'" . csharp-mode))
  :hook (csharp-mode . init/mode/csharp)
  :config
  (add-hook 'before-save-hook 'lsp-format-buffer)
  )

;;; csharp.el ends here
