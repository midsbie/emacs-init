;;; init-make-mode.el --- Configure Makefile mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/make-mode/enable ()
  (init/common-nonweb-programming-mode))

;; Mode is `makefile-mode' in make-mode.el, provides `make-mode'.
(use-package make-mode
  :ensure nil
  :hook ((makefile-mode . init/make-mode/enable)))

(provide 'init-make-mode)

;;; init-make-mode.el ends here
