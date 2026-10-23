;;; my-eslint.el --- ESLint utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

;;;###autoload
(defun my/eslint-fix ()
  "Apply ESLint fixes to the visited file and refresh the buffer.

ESLint runs synchronously on the file on disk, so a modified buffer is
refused rather than reverted.  The first ESLint on the buffer's
`exec-path' runs from the buffer's directory, so the nearest
installation and configuration apply.  Signal an error when ESLint is
missing or fails to run."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (when (buffer-modified-p)
    (user-error "Save the buffer before applying ESLint fixes"))
  (let ((eslint (or (executable-find "eslint" t)
                    (user-error "ESLint not found")))
        (file (file-local-name buffer-file-name)))
    (with-temp-buffer
      ;; The temporary buffer inherits `default-directory', from which
      ;; ESLint 9 looks up its configuration.  ESLint exits with 1 when
      ;; problems remain after fixing.
      (let ((status (process-file eslint nil t nil "--fix" file)))
        (unless (memq status '(0 1))
          (error "ESLint failed with status %s: %s"
                 status (string-trim (buffer-string))))))
    (unless (verify-visited-file-modtime)
      (revert-buffer t t t))))

(provide 'my-eslint)

;;; my-eslint.el ends here
