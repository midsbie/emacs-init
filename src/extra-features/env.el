;;; env.el --- Extra features for shell environment management

;; Copyright (C) 2023  Miguel Guedes

;; Author: Miguel Guedes <miguel@softgeist.com>
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

(defun el/set-env-vars-from-file (filename)
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

;;; env.el ends here
