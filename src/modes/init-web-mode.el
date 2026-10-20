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

(defun init/web-mode/before-save ()
  ;; Set web-mode's content-type to "typescript" when editing TS source files or
  ;; it'll default to JSX, causing prettier to error out on Typescript-specific
  ;; syntax.  This cannot be set preemptively because it results in web-mode not
  ;; rendering the JSX markup in the buffer correctly.
  (when (eq major-mode 'web-mode)
    (let ((ext (file-name-extension (buffer-file-name))))
      (cond ((not ext) t)
            ((eq (string-match "^tsx?$" ext) 0)
             (setq web-mode-content-type "typescript")))))
  )

(defun init/web-mode/after-save ()
  ;; Undoing the setting made in `init/web-mode/before-save', if applicable.
  (when (eq major-mode 'web-mode)
    (let ((ext (file-name-extension (buffer-file-name))))
      (cond ((not ext) t)
            ((eq (string-match "^tsx?$" ext) 0)
             (setq web-mode-content-type "jsx")))))
  )

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

  (advice-add 'prettier--maybe-prettify-on-save :before #'init/web-mode/before-save)
  (advice-add 'prettier--maybe-prettify-on-save :after #'init/web-mode/after-save)
  )

(provide 'init-web-mode)

;;; init-web-mode.el ends here
