;;; eglot.el --- Customises the Eglot package

;; Copyright (C) 2022  Miguel Guedes

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

(defvar init/typescript-server-location
  (expand-file-name
   "~/.emacs.d/.cache/lsp/npm/typescript-language-server/bin/typescript-language-server"))

(defun init/eglot ()
  "Configure `eglot' package."

  ;; Do not persist server events for better performance
  (setq eglot-events-buffer-size 0)

  ;; Configure typescript server location by replacing setting for the js and
  ;; typescript modes
  ;;
  ;; Note that this is currently disabled since we can simply symlink the server
  ;; in the bin directory or some other location that is in PATH.
  ;;
  ;;   (setcdr (assoc '(js-mode typescript-mode) eglot-server-programs)
  ;;           (list init/typescript-server-location "--stdio"))

  ;; Configure `eglot' to support Typescript source files when edited in
  ;; `web-mode'.
  (if (assoc 'web-mode eglot-server-programs)
      (setcdr (assoc 'web-mode eglot-server-programs)
              '("typescript-language-server" "--stdio"))
    (add-to-list 'eglot-server-programs
                 '(web-mode . ("typescript-language-server" "--stdio"))))
  )

(defun init/eglot/config ()
  "Configure `eglot' when enabled in a buffer.")

(use-package eglot
  :hook (eglot-managed-mode . init/eglot/config)

  :bind (
         :map eglot-mode-map
              ("C-c l a a" . eglot-code-actions)
              ("C-c l r r" . eglot-rename)
              )

  :config (init/eglot))

;;; eglot.el ends here
