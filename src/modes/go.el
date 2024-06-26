;;; go.el --- Configures `go-mode'

;; Copyright (C) 2015-2023  Miguel Guedes

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

;;; Log:
;;
;; 260323 Reverted to using `lsp' when js, typescript and web modes were
;;        switched over.
;;
;; 280722 Switched exclusively to `eglot'
;;
;; 130720 Remove dependency on go-autocomplete in favour of lsp-server and
;;        company-mode.
;;

;;; Code:

(defun init/go-mode ()
  "One-time configuration sequence for `go-mode'."

  ;; Warn when "go" binary not found in path.
  (unless (executable-find "go")
      (warn "Unable to find 'go' binary"))

  ;; Make sure the gopls binary is available otherwise eglot will fail to run.
  (unless (executable-find "gopls")
      (warn "Unable to find 'gopls' for Go source files: eglot will not run")))

(defun init/go-mode/mode ()
  "Configure `go-mode' major mode."
  (init/common-nonweb-programming-mode)

  ;; Customize compile command to run go build
  (unless (string-match "go build" compile-command)
    (setq-local compile-command "go build -v "))

  ;; Mode-specific settings
  (setq-local tab-width 8)
  (setq-local fill-column 100)

  (auto-fill-mode -1))

(defun init/go-mode/format ()
  "Format buffer just before saving.
Note that `lsp-format-buffer' should not be called here as this
is automatically handled by a hook function setup elsewhere.
Only additional formatting should be carried out in this
function."
  (ignore-errors
    (lsp-organize-imports)))

(use-package go-mode
  :mode (("\\.go\\'" . go-ts-mode))
  :hook ((go-mode . init/go-mode/mode)
         (go-ts-mode . init/go-mode/mode)
         (before-save . init/go-mode/format))
  :config (init/go-mode)
  :bind (
         (:map go-mode-map
               ("C-c C-c" . compile)
               ("C-c C-r" . go-remove-unused-imports))))

;;; go.el ends here
