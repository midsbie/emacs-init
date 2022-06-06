;;; orderless.el --- Configures the orderless package

;; Copyright (C) 2022  Miguel Guedes

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
  :init
  :config
  ;; Taken from:
  ;; https://www.reddit.com/r/emacs/comments/o9pue1/comment/h3cpr9x/
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  (setq completion-styles '(orderless basic substring flex initials
                                      partial-completion))

  (setq completion-category-defaults nil
        orderless-matching-styles    '(orderless-literal orderless-regexp))

  (setq completion-category-overrides
        ;; The documentation states that "basic completion style needs to be
        ;; tried first (not as a fallback) for TRAMP hostname completion to
        ;; work", however the partial-completion style seems to work just as
        ;; fine.
        '((file (styles partial-completion orderless+initialism))
          (buffer (styles orderless+initialism))
          (consult-multi (styles orderless+initialism))
          (command (styles orderless+initialism))
          (variable (styles orderless+initialism))
          (symbol (styles orderless+initialism))))
  )

;;; orderless.el ends here
