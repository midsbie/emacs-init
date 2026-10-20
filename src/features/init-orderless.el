;;; init-orderless.el --- Configures the orderless package  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
