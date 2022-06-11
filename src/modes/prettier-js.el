;;; prettier-js.el --- Configures the prettier-js minor mode

;; Copyright (C) 2017-2018  Miguel Guedes

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

;; Try the following steps when troubleshooting issues:
;;
;; (1) `prettier-restart' after making changes to a repository's `.prettierrc'
;;     file.
;;
;; (2) `prettier-info' to verify prettier's configuration.
;;
;; Note that `prettier-mode' now bundles a base64-encoded compressed version of
;; prettier (follow references to file "prettier-el.js.gz.base64" in
;; `prettier--create-process') rather than execute prettier's script and that it
;; may produce different results compared to a project's installed version if
;; the versions mismatch significantly.

;;; Code:

(defgroup init/prettier-mode nil
  "Customisations for `prettier-mode'."
  :group 'init
  :prefix "init/prettier-mode")

(defcustom init/enable-prettier-mode nil
  "Enable `prettier-mode'.

Specifies whether `prettier-mode' should always be enabled in
all JS/X buffers."
  :group 'init/prettier-mode
  :type 'boolean)

(defun enable-prettier-mode-maybe ()
  "Turn on `prettier-mode' selectively.

If the file associated with the current buffer is contained in a
repository in which the prettier executable can be found in the
relative path \"node_modules/.bin/prettier\", `prettier-mode' is
enabled.

The one exception to this rule is when the file is inside a
\"node_modules\"."
  (when (and (or init/enable-prettier-mode
                 (locate-file-in-dominating-node-modules ".bin/prettier" buffer-file-name))
             (not (dir-is-parent-p "node_modules" buffer-file-name)))
      (prettier-mode 1)))

(use-package prettier
  :diminish "Pr"
  :init
  ;; Resolve transient hang when saving files.
  ;; Ref: https://github.com/jscheid/prettier.el/issues/34#issuecomment-657508597
  (unless (getenv "NODE_PATH")
    (setenv "NODE_PATH" "/usr/lib/node_modules"))

  :hook ((
          ;; Actively used modes
          html-mode css-mode scss-mode js-mode js-jsx-mode typescript-mode
                    web-mode
                    ;; Deprecated modes
                    js2-mode)
         . enable-prettier-mode-maybe))

;;; prettier-js.el ends here
