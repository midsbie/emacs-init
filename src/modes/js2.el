;;; js2.el --- Configures `js2-mode'

;; Copyright (C) 2015  Miguel Guedes

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

;;; Code:

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Disabled: js2-mode not quite adequate enough to handle React's jsx content.
;(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
;(add-to-list 'auto-mode-alist '("\\.react.js\\'" . js2-jsx-mode))

(eval-after-load "js2-mode"
  '(progn
     (require 'js-comint)

     (setq-default js2-basic-offset    2

                   ;; FIXME: disabled because it is supposed to be set via the
                   ;; `customize-group' defun.
                   ;; js2-bounce-indent-p t

                   js2-include-browser-externs t
                   js2-include-node-externs    t
                   )

     (add-hook 'js2-mode-hook  'init-common-programming)
     (add-hook 'js2-mode-hook  'init-js2-mode)))

(defun init-js2-mode ()
  "Customise js2-mode.
In particular, add responsive logic to indent the current line
whenever the dot character (.) is typed on a continued
expression."
  (setq comment-start "/* "
        comment-end   " */")

  (local-set-key "."  '(lambda ()
                         (interactive)
                         (insert-char ?.)
                         (when (js2-continued-expression-p)
                           (indent-for-tab-command))))

  (local-set-key "}"  '(lambda ()
                         (interactive)
                         (insert-char ?})
                         (indent-for-tab-command)))

  ;; Bounce indenting support.
  (local-set-key [C-tab]           'js2-indent-bounce)
  (local-set-key [C-S-iso-lefttab] 'js2-indent-bounce-backward)

  ;; TODO: we should be replacing the default behaviour of the F3 key.
  (local-set-key [f3]
                 '(lambda ()
                    (interactive)
                    (unless (and next-error-function
                                 (not (string= (type-of (js2-next-error))
                                               "string")))
                      (flycheck-next-error))))

  ;; Set cool keyboard shortcuts to work in conjunction with `js-comint'.
  (local-set-key "\C-x\C-e" 'js-send-last-sexp)
  (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
  (local-set-key "\C-cb" 'js-send-buffer)
  (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
  (local-set-key "\C-cl" 'js-load-file-and-go)

  ;; Customize faces.  For some strange reason, `js2-mode' specifies the
  ;; default face for the function-call face.  We set it to font-locks's
  ;; default.
  (custom-set-faces
   '(js2-function-call ((t (:inherit font-lock-function-name-face))))
   ; The following leads to excessive highlighting; disabled.
;   '(js2-object-property ((t (:inherit font-lock-variable-name-face))))
   )

  ;; Only set path to the dominating .jshintrc if one actually was found.
  (let* ((loc (locate-dominating-file
               default-directory ".jshintrc")))
    ;; from: https://github.com/Wilfred/flymake-jshint.el/issues/1
    (setq-local jshint-configuration-path
                (and loc (expand-file-name ".jshintrc" loc))))
  (flymake-jshint-load)
  (init-js2-mode/load-jshint-globals)

  ;; Run hook after local variables loaded.
  (add-hook 'js2-mode-local-vars-hook 'init-js2-mode/load-local-vars)
  )

(defun init-js2-mode/load-jshint-globals (&optional file)
  "Load the `globals` section in the project's .jshintrc file.

The `globals` section is then appended to the buffer local
`js2-additional-externs' list.

If FILE is not specified, `jshint-configuration-path' is used instead."
  (unless file
    (setq file jshint-configuration-path))
  (when (and (or (eq major-mode 'js2-mode)
                 (eq major-mode 'js2-jsx-mode)) file)
    (let ((globals nil))
      (with-temp-buffer
        (insert-file-contents file)
        (when (search-forward-regexp "\"globals\"\s*:\s*{\s*\\([^}]+\\)\s*}"
                                     nil t)
          (let ((matches (split-string (match-string 1) ",")))
            (dolist (elt matches)
              (when (string-match "^\s*\"\\([^\"]+\\)\"\s*:\s*\\([a-z]+\\)" elt 0)
                (let ((name (match-string 1 elt))
                      (value (match-string 2 elt)))
                  (when (string= value "true")
                    (push name globals))))))))
      (setq js2-additional-externs (append js2-additional-externs globals))
      (delete-dups js2-additional-externs))))

(defun init-js2-mode/load-local-vars ()
  "Map the value of `c-basic-offset' to `js2-basic-offset'."
  (when file-local-variables-alist
    (dolist (elt file-local-variables-alist)
      (let* ((var (car elt))
             (val (cdr elt)))
        (cond ((eq var 'c-basic-offset)
               (setq js2-basic-offset val))
        )))))

;;; js2.el ends here
