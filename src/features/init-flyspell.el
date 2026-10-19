;;; init-flyspell.el --- Configure flyspell -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configuration for the flyspell library.

;;; Code:

(use-package flyspell
  :ensure nil
  :diminish
  :config
  ;; Keep C-; available for Embark.
  (define-key flyspell-mode-map (kbd "C-;") nil))

(provide 'init-flyspell)

;;; init-flyspell.el ends here
