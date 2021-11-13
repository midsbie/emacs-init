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

;;

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

;; Default parameters to pass to prettier.
(defcustom init/prettier-projects nil
  "List of projects to enable prettier-mode for."
  :group 'init/prettier-mode
  :type '(repeat string))

(defun enable-prettier-mode-maybe ()
  "Turn on `prettier-mode' selectively.

If the file associated with current buffer is contained by one of
the directories in `init/prettier-projects',
`prettier-mode' is also enabled.

The one exception to the rules above is when the file is inside a
\"node_modules\"."
  (when init/enable-prettier-mode
    (prettier-mode 1))
  (unless (and (boundp 'prettier-mode) prettier-mode)
    (let* ((path (file-name-directory (directory-file-name  buffer-file-name)))
           (parent path)
           (validp t))
      (while (and (not (string= path "/")) validp)
        (if (string= (file-name-nondirectory (directory-file-name path)) "node_modules")
            (setq validp nil)
          (setq path (file-name-directory (directory-file-name path)))))
      (when validp
        (dolist (dir init/prettier-projects)
          (when (file-in-directory-p buffer-file-name dir)
            (add-node-modules-path)
            (prettier-mode 1)))))))

(use-package prettier
  :init
  ;; Resolve transient hang when saving files.
  ;; Ref: https://github.com/jscheid/prettier.el/issues/34#issuecomment-657508597
  (unless (getenv "NODE_PATH")
    (setenv "NODE_PATH" "/usr/lib/node_modules"))
  :hook ((
          ;; Actively used modes
          html-mode css-mode scss-mode js-mode js-jsx-mode typescript-mode
                    ;; Deprecated modes
                    js2-mode web-mode)
         . enable-prettier-mode-maybe))

;;; prettier-js.el ends here
