;;; js.el --- Configures `js-mode'

;; Copyright (C) 2015-2020  Miguel Guedes

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
;; xxxxxx Using `js2-mode' instead.
;; xxxxxx Using `web-mode' as the development on the above mode has been
;;        discontinued and JSX support isn't good enough.
;;
;; 120519 Considered enabling `js-jsx-mode' as it now seems to handle JSX
;;        source files correctly and seems faster than `web-mode', however
;;        syntax highlighting isn't as good and useful as the latter's.
;;
;; 140520 Attempted to switch to `js-mode' and `js-jsx-mode' for all Javascript
;;        development, as it is faster, more responsive and does not suffer
;;        from the quirks that `web-mode' does, however syntax highlighting is
;;        not great.  Staying with web-mode for the time being.

;;

;;; Code:

;; Uncomment the following code if enabling:
;;
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\?\\'" . js-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.react.js\\'" . js-jsx-mode))

;; (eval-after-load 'js-mode
;;   '(progn
;;      (add-hook 'js-mode-hook  'init/js-mode)
;;      (add-hook 'js-mode-hook  'init/common-web)))

;; (eval-after-load 'js-jsx-mode
;;   '(progn
;;      (add-hook 'js-jsx-mode-hook  'init/js-mode)
;;      (add-hook 'js-jsx-mode-hook  'init/common-web)))

(defun init/js-mode ()
  "Initialise modes related to Javascript development."
  (electric-indent-mode)
  ; FIXME: this does not seem to have any effect in JSX mode:
  (setq comment-start "/* "
        comment-end   " */")

  (setq-local fill-column 99)

  (local-set-key (kbd "M-a") 'c-beginning-of-statement)
  (local-set-key (kbd "M-e") 'c-end-of-statement))

;;; js.el ends here
