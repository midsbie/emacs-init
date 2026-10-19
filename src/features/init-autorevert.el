;;; init-autorevert.el --- Configure autorevert -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configuration for the autorevert library.

;;; Code:

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :config
  ;; Revert buffer if file associated with it changes outside of emacs.
  (global-auto-revert-mode 1))

(provide 'init-autorevert)

;;; init-autorevert.el ends here
