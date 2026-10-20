;;; init-orderless.el --- Configure orderless  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Documentation: https://github.com/oantolin/orderless

;;; Code:

(use-package orderless
  :demand
  :custom
  (completion-styles '(orderless basic))
  (orderless-matching-styles '(orderless-literal orderless-regexp))
  ;; Request initialisms with a comma and flex matching with a tilde.
  ;; Ordinary file prompts need path expansion and TRAMP completion;
  ;; project prompts match a supplied list of project-relative paths.
  (completion-category-overrides
   '((file (styles partial-completion orderless))
     (project-file (styles orderless))))
  ;; Emacs 31: allow fragments within each partial path component.
  (completion-pcm-leading-wildcard t))

(provide 'init-orderless)

;;; init-orderless.el ends here
