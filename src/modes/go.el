;;; go.el --- Configures `go-mode'

;; Copyright (C) 2015-2022  Miguel Guedes

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
;; 13/07/20 Remove dependency on go-autocomplete in favour of lsp-server and
;;          company-mode.

;;

;;; Code:

(defun init/go-mode ()
  "One-time configuration sequence for `go-mode'."

  ;; Make sure $HOME/go/bin is in the PATH env var otherwise eglot will fail to
  ;; run.
  (let ((gobin (expand-file-name "go/bin" (getenv "HOME"))))
    (unless (string-match gobin (getenv "PATH"))
      (error "Unable to find gopls in PATH: eglot will fail to run"))))

(defun init/go-mode/config ()
  "Configure `go-mode' major mode."
  (init/common-nonweb-programming-mode)
  (go-eldoc-setup)

  ;; Customize compile command to run go build
  (unless (string-match "go build" compile-command)
    (setq-local compile-command "go build -v"))

  ;; Mode-specific settings
  (setq-local tab-width 8)

  ;; (add-hook 'before-save-hook 'gofmt-before-save nil t)
  (eglot-ensure)
  )

(use-package go-mode
  :mode ("\\.go\\'")
  :hook ((go-mode . init/go-mode/config))
  :init (init/go-mode)
  :bind ((:map go-mode-map
               ("C-c C-c" . compile)
               ("C-c C-r" . go-remove-unused-imports))))

;;; go.el ends here
