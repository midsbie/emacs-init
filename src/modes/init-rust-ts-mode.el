;;; init-rust-ts-mode.el --- Configure Rust modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/rust-ts-mode/config ()
  "One-time configuration sequence for `go-mode'."

  ;; Warn when "go" binary not found in path.
  (unless (executable-find "rustc")
      (warn "Unable to find 'rustc' binary"))

  ;; Make sure the gopls binary is available otherwise eglot will fail to run.
  (unless (executable-find "rust-analyzer")
      (warn "Unable to find 'rust-analyzer' for Rust source files: eglot will not run")))

(defun init/rust-ts-mode/enable ()
  "Configure `rust-mode' major mode."
  (init/common-nonweb-programming-mode)

  ;; Customize compile command to run go build
  (unless (string-match "cargo build" compile-command)
    (setq-local compile-command "cargo build "))

  ;; Mode-specific settings
  (setq-local fill-column 100))

(use-package rust-ts-mode
  :ensure nil
  :mode (("\\.rs\\'" . rust-ts-mode))
  :hook ((rust-mode rust-ts-mode) . init/rust-ts-mode/enable)
  :config (init/rust-ts-mode/config))

(provide 'init-rust-ts-mode)

;;; init-rust-ts-mode.el ends here
