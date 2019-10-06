;;; init.el --- Backwards-compatibility definitions

;; Copyright (C) 2019 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; URL:
;; Keywords: tools
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:

;;; Code:

;; `define-symbol-prop' not available until emacs 26
(unless (fboundp 'define-symbol-prop)
  (defun define-symbol-prop (symbol prop val)
    "Define the property PROP of SYMBOL to be VAL.
This is to `put' what `defalias' is to `fset'."
    ;; Can't use `cl-pushnew' here (nor `push' on (cdr foo)).
    ;; (cl-pushnew symbol (alist-get prop
    ;;                               (alist-get 'define-symbol-props
    ;;                                          current-load-list)))
    (let ((sps (assq 'define-symbol-props current-load-list)))
      (unless sps
        (setq sps (list 'define-symbol-props))
        (push sps current-load-list))
      (let ((ps (assq prop sps)))
        (unless ps
          (setq ps (list prop))
          (setcdr sps (cons ps (cdr sps))))
        (unless (member symbol (cdr ps))
          (setcdr ps (cons symbol (cdr ps))))))
    (put symbol prop val)))

;; It seems newer Emacs versions automatically enable electric indentation
;; mode.  Seeing as we don't like that, it is disabled right here.  Should also
;; be backwards compatible.
(when (and (> emacs-major-version 24) (fboundp 'electric-indent-mode))
  (electric-indent-mode -1))

;;; compat.el ends here
