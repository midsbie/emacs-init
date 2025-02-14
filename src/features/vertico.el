;;; vertico.el --- Configures the Vertico package

;; Copyright (C) 2022-2025  Miguel Guedes

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

;; This module configures the Vertico package.
;;
;; The configuration used initially was lifted verbatim from
;; https://github.com/minad/vertico#configuration .

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

;;; vertico.el ends here
