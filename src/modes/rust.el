;;; rust.el --- Configures `rust-mode'

;; Copyright (C) 2025  Miguel Guedes

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

;;; Code:

(defun init/rust-mode/config ()
  "One-time configuration sequence for `go-mode'."

  ;; Warn when "go" binary not found in path.
  (unless (executable-find "rustc")
      (warn "Unable to find 'rustc' binary"))

  ;; Make sure the gopls binary is available otherwise eglot will fail to run.
  (unless (executable-find "rust-analyzer")
      (warn "Unable to find 'rust-analyzer' for Rust source files: eglot will not run")))

(defun init/rust-mode/enable ()
  "Configure `rust-mode' major mode."
  (init/common-nonweb-programming-mode)

  ;; Customize compile command to run go build
  (unless (string-match "cargo build" compile-command)
    (setq-local compile-command "cargo build "))

  ;; Mode-specific settings
  (setq-local fill-column 100)

  (auto-fill-mode -1))

(use-package rust-ts-mode
  :mode (("\\.rs\\'" . rust-ts-mode))
  :hook ((rust-mode rust-ts-mode) . init/rust-mode/enable)
  :config (init/rust-mode/config))

;;; rust.el ends here
