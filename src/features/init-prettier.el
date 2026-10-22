;;; init-prettier.el --- Configure prettier  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

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

(defvar-local init/prettier/enable-timer nil
  "Idle timer scheduled to turn on `prettier-mode' in this buffer.")
;; Survive major mode changes so that setup under a later mode can cancel it.
(put 'init/prettier/enable-timer 'permanent-local t)

(defun init/prettier/enable-in-buffer (buffer mode)
  "Turn on `prettier-mode' in BUFFER if it is live, still in MODE and eligible.

BUFFER is eligible when its repository has a local Prettier installation
\(the executable is found in the relative path
\"node_modules/.bin/prettier\") or `init/enable-prettier-mode' is non-nil,
and BUFFER is not itself inside node_modules."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq init/prettier/enable-timer nil)
      (when (and (eq major-mode mode)
                 (ignore-errors
                   (and (or init/enable-prettier-mode
                            (my/locate-file-in-dominating-node-modules
                             ".bin/prettier" default-directory))
                        (not (my/dir-is-parent-p "node_modules" default-directory)))))
        (prettier-mode 1)))))

(defun init/prettier/maybe-enable ()
  "Turn on `prettier-mode' selectively, once the buffer is idle.

Enabling is deferred to resolve an issue where some .prettierrc settings
are ignored when formatting the buffer on save.  Strangely this only
happens when LSP is active.  Repeated calls replace the pending request,
which `init/prettier/enable-in-buffer' drops if the major mode changed."
  (when (timerp init/prettier/enable-timer)
    (cancel-timer init/prettier/enable-timer))
  (setq init/prettier/enable-timer
        (run-with-idle-timer .1 nil #'init/prettier/enable-in-buffer
                             (current-buffer) major-mode)))

(use-package prettier
  :diminish " P"

  :hook ((
          ;; Actively used modes
          html-mode css-mode scss-mode js-mode js-jsx-mode typescript-mode
                    web-mode
                    ;; Tree-sitter modes
                    css-ts-mode typescript-ts-mode tsx-ts-mode js-ts-mode
                    ;; Deprecated modes
                    js2-mode)
         . init/prettier/maybe-enable))

(provide 'init-prettier)

;;; init-prettier.el ends here
