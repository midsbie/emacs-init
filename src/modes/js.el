;;; js.el --- Configures `js-mode'

;; Copyright (C) 2015-2021  Miguel Guedes

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
;;
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
;; 031221 Now using `js-mode' and `js-jsx-mode' for Javascript files.

;;

;;; Code:

(defun init/js-mode()
  "Initialise `js-mode'."
  ;; Supporting flow through `flow-minor-mode', although it is currently not in
  ;; use.
  (cl-pushnew 'company-flow company-backends))

(defun init/js-mode/config/determine-js-mode ()
  (if (flycheck-flow--predicate)
      (web-mode)
    (js-mode)))

(defun init/js-mode/config/determine-jsx-mode ()
  (if (flycheck-flow--predicate)
      (web-mode)
    (js-jsx-mode)))

(defun init/js-mode/config ()
  "Initialise modes related to Javascript development."

  ;; FIXME: this does not seem to have any effect in JSX mode:
  (setq-local comment-start "/* ")
  (setq-local comment-end   " */")
  (setq-local fill-column init/defaults/fill-column)

  (init/common-web-programming-mode)

  ;; Was activating flow minor mode previously:
  ;;
  ;;   (when (flycheck-flow--predicate)
  ;;     (flow-minor-mode 1))
  ;;
  ;; Then support transitioned to LSP for Flow source files and to eglot for all
  ;; other source files:
  ;;
  ;;   (cond
  ;;    ((flycheck-flow--predicate)
  ;;     (lsp-deferred))
  ;;    (t
  ;;     (eglot-ensure)))
  ;;
  ;; Now exclusively using eglot.
  (eglot-ensure))

(defun init/js-mode/toggle-mode ()
  "Switch to `web-mode'."
  (interactive)
  (web-mode))

(use-package js
  :after (company flycheck)
  :diminish "JS"
  :mode (("\\.js\\'" . init/js-mode/config/determine-js-mode)
         ("\\.jsx\\'" . init/js-mode/config/determine-jsx-mode))
  :hook (((js-mode js-jsx-mode) . init/js-mode/config))
  :bind (:map js-mode-map
              ("C-c C-c" . init/js-mode/toggle-mode))
  :init
  (init/js-mode))

;;; js.el ends here
