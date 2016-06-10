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

(eval-after-load 'go-mode
  '(progn
     (add-hook 'go-mode-hook  'init-go-mode)

     ;; Use goimports instead of go-fmt
     ;;
     ;; Note that this requires the executable `goimports' to be locatable in
     ;; the path.  goimports can be found at:
     ;; https://godoc.org/golang.org/x/tools/cmd/goimports
     (if (executable-find "goimports")
         (setq-default gofmt-command "goimports")
       (message "info: goimports not found and will be unavailable"))

     ;; Load go autocomplete
     (load-library "go-autocomplete")

     ;; Attempt to load Go Oracle.
     ;;
     ;; Note that this requires the `oracle.el' to be found in the directory
     ;; specified below.  oracle can be found at:
     ;; https://godoc.org/golang.org/x/tools/cmd/oracle
     ;;
     ;; TODO: it is ugly that we're loading from the home directory.  This
     ;; should perhaps be in the user's .emacs file.
     (let ((file (expand-file-name
                  "~/go/src/golang.org/x/tools/cmd/oracle/oracle.el")))
       (if (not (file-exists-p file))
           (message "info: go oracle not found and will be unavailable")
         (load file)))

     (if (not (executable-find "go"))
         (error "GOROOT environment variable not set and go\
 executable not found")
       (let ((env (shell-command-to-string "go env")))
         (unless (getenv "GOROOT")
           (init-go-mode/setenv "GOROOT" env))
         (unless (getenv "GOPATH")
           (setenv "GOPATH" (expand-file-name "~/go"))
           (message "info: set default GOPATH" name))))
     ))

(defun init-go-mode ()
  "Initialise modes related to Go development."
  (init-common-programming)
  (go-eldoc-setup)
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v"))              ; removed: "&& go test -v && go vet"

  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
  (local-set-key (kbd "C-c i")   'go-goto-imports)
  (local-set-key (kbd "M-.")     'go-jump))

(defun init-go-mode/setenv (name env)
  "Extract environment variable given by NAME from go's ENV."
  (if (not (string-match (concat name "=\"\\([^\"]+\\)\"") env))
      (error (concat "Failed to extract " name))
    (setenv name (match-string 1 env))
    (message "info: set %s" name)))

;;; go.el ends here
