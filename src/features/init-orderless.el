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
  (completion-styles '(orderless basic substring flex initials partial-completion))
  (orderless-matching-styles '(orderless-literal orderless-regexp))
  ;; partial-completion handles file and TRAMP completion before Orderless.
  (completion-category-overrides
   '((file (styles partial-completion orderless+initialism))
     (buffer (styles orderless+initialism))
     (consult-multi (styles orderless+initialism))
     (command (styles orderless+initialism))
     (variable (styles orderless+initialism))
     (symbol (styles orderless+initialism))))
  :config
  ;; Register the named style before any completion can use these settings.
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
  ;; This is an ordinary variable, not a user option.
  (setq completion-category-defaults nil))

(provide 'init-orderless)

;;; init-orderless.el ends here
