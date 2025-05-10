;;; flymake.el --- Configures the flymake feature

;; Copyright (C) 2022-2025  Miguel Guedes

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

(defun init/flymake/enable ()
  "Configure flymake.

This function disables `flycheck-mode', if enabled, and
configures `flymake-eslint' correctly if it finds `eslint' in the
variable `exec-path'."
  (when (and (eglot-managed-p) (boundp 'flycheck-mode) flycheck-mode)
    (flycheck-mode -1))

  (flymake-diagnostic-at-point-mode 1))

(use-package flymake
  :demand
  :hook ((flymake-mode . init/flymake/enable)))

(use-package flymake-diagnostic-at-point
  :demand
  :after flymake
  :bind ((:map flymake-mode-map
               ("C-c ! p" . flymake-goto-prev-error)
               ("C-c ! n" . flymake-goto-next-error)
               ("C-c ! l" . flymake-show-project-diagnostics)
               ("C-c ! b" . flymake-show-buffer-diagnostics)
               ("C-c ! c" . flymake-start))))

(defun init/flymake-ruff-load ()
  "Load the backend for ruff under flymake for the current buffer.

Loading is deferred due to an unidentified interaction preventing ruff
to be added as a flymake backend."
  (run-with-idle-timer 1 nil #'(lambda()
                                 (flymake-ruff-load)
                                 ;; First buffer check doesn't trigger somehow
                                 (flymake-start))))

(use-package flymake-ruff
  :ensure t
  :hook (((python-mode python-ts-mode) . init/flymake-ruff-load))
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

;;; flymake.el ends here
