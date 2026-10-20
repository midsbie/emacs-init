;;; my-env.el --- Shell environment utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

;;;###autoload
(defun my/set-env-vars-from-file (filename)
  "Set environment variables from a .env file specified by FILENAME."
  (interactive "FSelect .env file: ")
  (with-temp-buffer
    (insert-file-contents filename)
    (dolist (line (split-string (buffer-string) "\n" t))
      (when (and (string-match "=" line)
                 (not (string-match "^#" line)))
        (let* ((key-value (split-string line "="))
               (key (car key-value))
               ; Handling case where the value part given by (cdr key-value)
               ; contained values with = characters that have been exploded into
               ; list elements. We use mapconcat to reassemble back to string
               ; form.
               (value (mapconcat 'identity (cdr key-value) "=")))
          (setenv key value)
          (when (equal key "PATH")
            (setq exec-path (append (parse-colon-path value) exec-path))))))))

(provide 'my-env)

;;; my-env.el ends here
