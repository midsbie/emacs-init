;;; init-go-ts-mode.el --- Configure Go modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/go-ts-mode/config ()
  "One-time configuration sequence for `go-mode'."

  ;; Warn when "go" binary not found in path.
  (unless (executable-find "go")
      (warn "Unable to find 'go' binary"))

  ;; Make sure the gopls binary is available otherwise eglot will fail to run.
  (unless (executable-find "gopls")
      (warn "Unable to find 'gopls' for Go source files: eglot will not run")))

(defun init/go-ts-mode/enable ()
  "Configure `go-mode' major mode."
  (init/common-nonweb-programming-mode)

  ;; Customize compile command to run go build
  (unless (string-match "go build" compile-command)
    (setq-local compile-command "go build -v "))

  ;; Mode-specific settings
  (setq-local tab-width 8)
  (setq-local fill-column 100))

(use-package go-ts-mode
  :ensure nil
  :mode (("\\.go\\'" . go-ts-mode))
  :hook ((go-mode go-ts-mode) . init/go-ts-mode/enable)
  :config (init/go-ts-mode/config))

(provide 'init-go-ts-mode)

;;; init-go-ts-mode.el ends here
