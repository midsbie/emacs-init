;;; init-elisp-mode.el --- Configure Emacs Lisp mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/elisp-mode/enable ()
  "Initialise modes related to ELISP development."

  (init/common-programming-mode)
  (setq-local fill-column init/defaults/fill-column/narrow))

(use-package elisp-mode
  :ensure nil
  :hook ((emacs-lisp-mode lisp-mode) . init/elisp-mode/enable))

(provide 'init-elisp-mode)

;;; init-elisp-mode.el ends here
