;;; init-savehist.el --- Configure savehist -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configuration for the savehist library.

;;; Code:

(use-package savehist
  :ensure nil
  :config
  (savehist-mode))

(provide 'init-savehist)

;;; init-savehist.el ends here
