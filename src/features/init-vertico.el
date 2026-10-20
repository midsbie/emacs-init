;;; init-vertico.el --- Configure vertico  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configuration adapted from https://github.com/minad/vertico#configuration.

;;; Code:

(use-package vertico
  :custom
  (vertico-count 20)  ; Show more candidates
  ;; (vertico-resize t)  ; Grow and shrink the Vertico minibuffer
  ;; (vertico-scroll-margin 0) ; Different scroll margin
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (vertico-cycle t)

  :config
  (vertico-mode))

(provide 'init-vertico)

;;; init-vertico.el ends here
