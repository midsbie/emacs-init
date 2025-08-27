;;; js.el --- Configures Javascript buffers

;; Copyright (C) 2015-2025  Miguel Guedes

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
;;

;;; Log:
;;
;; 070423 Now using tree-sitter with JSX syntax highlighting provided by the
;;        `tsx-ts-mode'.  The default LSP flow activation function is overriden
;;        to make sure it works with tsx-ts-mode.
;;
;; 260323 Reverted to using `lsp' as an experiment to solve weird performance
;;        issues under eglot.
;;
;; 031221 Now using `js-mode' and `js-jsx-mode' for Javascript files.
;;
;; 140520 Attempted to switch to `js-mode' and `js-jsx-mode' for all Javascript
;;        development, as it is faster, more responsive and does not suffer from
;;        the quirks that `web-mode' does, however syntax highlighting is not
;;        great.  Staying with web-mode for the time being.
;;
;; 120519 Considered enabling `js-jsx-mode' as it now seems to handle JSX source
;;        files correctly and seems faster than `web-mode', however syntax
;;        highlighting isn't as good and useful as the latter's.
;;
;; xxxxxx Using `web-mode' as the development on the above mode has been
;;        discontinued and JSX support isn't good enough.
;;
;; xxxxxx Using `js2-mode' instead.
;;

;;; Code:

(defun init/js/lsp-clients-flow-activate-p (file-name _mode)
  "Check if the Flow language server should be enabled for a
particular FILE-NAME and MODE."
  (and (derived-mode-p 'js-mode 'web-mode 'js2-mode 'flow-js2-mode 'rjsx-mode
                       'js-ts-mode 'tsx-ts-mode)
       (not (derived-mode-p 'json-mode))
       (or (lsp-clients-flow-project-p file-name)
           (lsp-clients-flow-tag-file-present-p file-name))))

(defun init/determine-js-mode ()
  ;; Do not add `:after flycheck-flow` clause to the use-package declaration
  ;; below or it'll have unintended side-effects.  The biggest is that
  ;; use-package won't run until `flycheck' is loaded, causing JS buffers to be
  ;; misconfigured.
  (when (not (fboundp 'flycheck-flow--predicate))
    (require 'flycheck-flow))

  (if (flycheck-flow--predicate)
      (tsx-ts-mode)
    ;; Don't use typescript-ts-mode as it will cause the tsserver to type-check
    ;; Javascript code.
    (js-ts-mode)))

(defun init/js-mode/enable ()
  "Configures modes related to Javascript development."
  (init/common-web-programming-mode))

(use-package lsp
  :config
    (advice-add 'lsp-clients-flow-activate-p
                :override #'init/js/lsp-clients-flow-activate-p))

(use-package js
  :diminish "JS"
  :mode (("\\.jsx?\\'" . init/determine-js-mode)
         ("\\.mjs\\'" . init/determine-js-mode))
  :hook (((js-ts-mode js-mode js-jsx-mode) . init/js-mode/enable)))

;;; js.el ends here
