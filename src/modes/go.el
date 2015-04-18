;;; go.el --- Configures `go-mode'

;; Copyright (C) 2015  Miguel Guedes

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

(require 'go-autocomplete)

(add-hook 'go-mode-hook  'init-go)

(defun init-go ()
  "Initialise modes related to Go development."
  init-common-programming
  (go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")

  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  )

(let ((file "$GOPATH/src/code.google.com/p/go.tools/cmd/oracle/oracle.el"))
  (if (not (file-exists-p file))
      (message "Go oracle not found")
    (load file)
    (add-hook 'go-mode-hook 'go-oracle-mode)))

;;; go.el ends here
