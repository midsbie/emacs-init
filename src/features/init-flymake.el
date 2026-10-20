;;; init-flymake.el --- Configure flymake  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/flymake/enable ()
  "Enable Flymake for Emacs Lisp buffers, which do not use Eglot."
  (flymake-mode 1))

(use-package flymake
  :ensure nil
  :demand
  :diminish
  :hook (((emacs-lisp-mode lisp-interaction-mode) . init/flymake/enable))
  :bind ((:map flymake-mode-map
               ("C-c ! p" . flymake-goto-prev-error)
               ("C-c ! n" . flymake-goto-next-error)
               ("C-c ! l" . flymake-show-project-diagnostics)
               ("C-c ! b" . flymake-show-buffer-diagnostics)
               ("C-c ! c" . flymake-start))))

(provide 'init-flymake)

;;; init-flymake.el ends here
