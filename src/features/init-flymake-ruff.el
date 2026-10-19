;;; init-flymake-ruff.el --- Configure flymake-ruff -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configuration for the flymake-ruff library.

;;; Code:

(defun init/flymake-ruff/enable ()
  "Add the ruff backend to flymake in the current Python buffer.

Eglot replaces `flymake-diagnostic-functions' when it starts managing a
buffer, dropping the ruff backend, so this also runs from
`eglot-managed-mode-hook' to add it back.  That hook runs in every
Eglot-managed buffer, so check the mode before loading ruff.

`pet' points other tools at the project's ruff but not `flymake-ruff',
so do that here."
  (when (derived-mode-p 'python-base-mode)
    (when-let* (((fboundp 'pet-executable-find))
                (ruff (pet-executable-find "ruff")))
      (setq-local flymake-ruff-program ruff))
    (flymake-ruff-load)
    (when flymake-mode
      (flymake-start))))

(use-package flymake-ruff
  :hook (((python-mode python-ts-mode eglot-managed-mode) . init/flymake-ruff/enable))
  ;; :vc (:url "git@github.com:midsbie/flymake-ruff.git"
  ;;             :rev :newest
  ;;             :branch "master")
  :config
  ;; Adding --no-cache to the default args to bypass ruff's internal cache and
  ;; ensure diagnostics match buffer contents.  Note that it may still defer to
  ;; its internal cache in some cases, in which case running ruff in watch mode
  ;; could prove to be a useful workaround since it will be constantly
  ;; repopulating the cache on every file change.  Command: `ruff check --watch
  ;; src/`.
  ;;
  ;; Another possibility is to hotfix the ruff code to never pass the
  ;; --stdin-filename arg. In `flymake-ruff--severity-for-code', locate the
  ;; block below:
  ;;              (args (if code-filename
  ;;                   (append args `("--stdin-filename" ,code-filename))
  ;;                 args))
  ;;
  ;; Change the first line to: (args (if (and nil code-filename)
  (setq flymake-ruff-program-args
        (let ((default-args flymake-ruff-program-args))
          (if (and (equal (car default-args) "check")
                   (not (member "--no-cache" default-args)))
              (append '("check" "--no-cache") (cdr default-args))
            default-args))))

(provide 'init-flymake-ruff)

;;; init-flymake-ruff.el ends here
