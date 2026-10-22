;;; init-flymake-eslint.el --- Configure flymake-eslint  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defvar flymake-eslint-executable-name)

(defun init/flymake-eslint/enable ()
  "Add the ESLint backend to Flymake in the current JavaScript buffer.

Flymake runs in these buffers once Eglot manages them.  Eglot replaces
`flymake-diagnostic-functions' when it starts managing a buffer,
dropping the ESLint backend, and restores the earlier value when it
stops, leaving Flymake on.  Running from `eglot-managed-mode-hook',
which fires on both transitions in every Eglot buffer, adds ESLint back
on reconnection and keeps it after Eglot shuts down.  Other modes and
buffers without an ESLint installation are left alone."
  (when (and flymake-mode (init/is-web-programming-mode-p))
    (require 'flymake-eslint)
    (when (executable-find flymake-eslint-executable-name t)
      (flymake-eslint-enable))))

(use-package flymake-eslint
  :hook (eglot-managed-mode . init/flymake-eslint/enable))

(provide 'init-flymake-eslint)

;;; init-flymake-eslint.el ends here
