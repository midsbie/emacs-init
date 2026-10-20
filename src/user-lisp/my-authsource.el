;;; my-authsource.el --- Authentication source utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'auth-source)

;;;###autoload
(defun my/read-authsource-secret (host user)
  "Retrieve the secret for a given host and user.

HOST is the host for which to retrieve the secret.
USER is the user for which to retrieve the secret.

Returns the secret value, encoded in UTF-8, or nil if no matching
authentication source is found."
  (when-let* ((secret
               (plist-get
                (car (auth-source-search
                      :host host
                      :user user
                      :require '(:secret)))
                :secret)))
    (if (functionp secret)
        (encode-coding-string (funcall secret) 'utf-8)
      secret)))

(provide 'my-authsource)

;;; my-authsource.el ends here
