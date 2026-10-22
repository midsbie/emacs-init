;;; init-web-mode.el --- Configure web-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; HTML files open in web-mode.  JavaScript and JSX use init-js.el;
;; TypeScript and TSX use init-typescript-ts-mode.el.

;;; Code:

(defun init/web-mode/enable ()
  "Configures enabled `web-mode'."
  (init/common-web-programming-mode)

  (make-local-variable 'web-mode-indentation-params)
  (make-local-variable 'web-mode-indent-offset)

  (setq-local web-mode-markup-indent-offset     2)
  (setq-local web-mode-css-indent-offset        2)
  (setq-local web-mode-code-indent-offset       2)
  (setq-local web-mode-block-padding            2)
  (setq-local web-mode-comment-style            2)
  (setq-local web-mode-attr-value-indent-offset 2)

  (setq-local web-mode-enable-css-colorization          t)
  (setq-local web-mode-enable-auto-pairing              t)
  (setq-local web-mode-enable-comment-interpolation     t)
  (setq-local web-mode-enable-current-element-highlight t)

  ;; javascript: make case indentation at the same level as the parent switch
  ;; statement.
  (add-to-list 'web-mode-indentation-params '("case-extra-offset" . nil)))

(defun init/web-mode/load-local-vars ()
  "Map the value of `c-basic-offset' to `web-mode-code-indent-offset'."
  (when file-local-variables-alist
    (dolist (elt file-local-variables-alist)
      (let* ((var (car elt))
             (val (cdr elt)))
        (cond ((eq var 'c-basic-offset)
               (setq-local web-mode-code-indent-offset val)))))))

(defvar web-mode-content-type)

(defun init/web-mode/prettify-as-typescript (prettify)
  "Call PRETTIFY with a TypeScript content type in web-mode TS and TSX buffers.
Web-mode does not give these files a TypeScript content type, so
Prettier would reject TypeScript syntax.  Keeping that content type
would make web-mode render JSX markup incorrectly, so the original is
restored however PRETTIFY exits."
  (if (and (eq major-mode 'web-mode)
           buffer-file-name
           (member (file-name-extension buffer-file-name) '("ts" "tsx")))
      (let ((web-mode-content-type "typescript"))
        (funcall prettify))
    (funcall prettify)))

(use-package web-mode
  :bind (:map web-mode-map
         ("." . my/insert-and-indent)
         ("}" . my/insert-and-indent)
         ;; Avoid inserting control characters inside JSX blocks.
         ("C-<left>" . backward-word)
         ("C-<right>" . forward-word))
  :mode ("\\.html?\\'")

  :hook ((web-mode-local-vars . init/web-mode/load-local-vars)
         (web-mode . init/web-mode/enable))

  :init
  ;; Set content type to jsx for source files with "js", "jsx" or "tsx" suffix.
  ;; Source: http://cha1tanya.com/2015/06/20/configuring-web-mode-with-jsx.html
  (setq-default web-mode-content-types-alist '(("jsx" . "\\.\\(jsx?\\|tsx\\)\\'")))

  (advice-add 'prettier--maybe-prettify-on-save :around
              #'init/web-mode/prettify-as-typescript))

(provide 'init-web-mode)

;;; init-web-mode.el ends here
