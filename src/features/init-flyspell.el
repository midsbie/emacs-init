;;; init-flyspell.el --- Configure flyspell  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package flyspell
  :ensure nil
  :diminish
  :config
  ;; Keep C-; available for Embark.
  (define-key flyspell-mode-map (kbd "C-;") nil))

(provide 'init-flyspell)

;;; init-flyspell.el ends here
