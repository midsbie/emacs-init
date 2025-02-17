;;; authsource.el --- Authentication sources in Emacs -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun my/read-authsource-secret (host user)
  "Retrieve the secret for a given host and user.

HOST is the host for which to retrieve the secret.
USER is the user for which to retrieve the secret.

Returns the secret value, encoded in UTF-8, or nil if no matching
authentication source is found."
  (if-let ((secret
            (plist-get
             (car (auth-source-search
                   :host host
                   :user user
                   :require '(:secret)))
             :secret)))
      (if (functionp secret)
          (encode-coding-string (funcall secret) 'utf-8)
        secret)
    ))

(provide 'authsource)

;;; authsource.el ends here
