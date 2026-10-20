;;; init-cc-mode.el --- Configure C and C++ modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/cc-mode/enable ()
  "Initialise modes related to C and C++ development."

  (init/common-nonweb-programming-mode)

  (setq-local comment-start "/* ")
  (setq-local comment-end   " */"))

(use-package cc-mode
  :ensure nil
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c-ts-mode)
         ("\\.cpp\\'" . c++-ts-mode)
         ("\\.cxx\\'" . c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode)
         ("\\.hxx\\'" . c++-ts-mode))

  ;; buftoggle not found
  ;;  :bind (("C-c o" . buftoggle))

  :hook ((c-ts-mode c++-ts-mode) . init/cc-mode/enable)

  :custom
  ;; Closest built-in match for the CC Mode "default" style
  ;; (config/init-programming.el): "bsd" keeps braces of substatements flush
  ;; with the statement (`substatement-open' 0).  `c-ts-indent-offset' is
  ;; already 2.
  (c-ts-mode-indent-style 'bsd)

  :init
  ;; Set environment for compilers to use, but only if not set.
  (when (not (getenv "CC"))
    (setenv "CC" (or (and (executable-find "clang") "clang") "gcc")))
  (when (not (getenv "CXX"))
    (setenv "CXX" (or (and (executable-find "clang++") "clang++") "g++"))))

(provide 'init-cc-mode)

;;; init-cc-mode.el ends here
