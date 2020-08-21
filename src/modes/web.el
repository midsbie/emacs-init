;;; web.el --- Configures `web-mode'

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
;; Do not forget to also make changes in the ~/.emacs file as it might be
;; forcefully enabling this (or some other) mode.
;;
;;; Log:
;; xxxxxx React's jsx files are best loaded under web mode.  However, we're now
;; using web-mode as the default mode for all things Javascript.
;;
;; 120519 Noting that `js-mode' and `js-jsx-mode' seem stable at the moment.
;; Unfortunately, `web-mode' still provides an overall better experience but
;; the two Emacs native modes should be kept regular under evaluation.
;;
;; 140520 Emacs supports JSX natively now, however syntax highlighting is still
;; not great.  Waiting until v27 and considering building from source.

;;

;;; Code:

(defun init/web-mode ()
  "Initialise `web-mode'."
  (init/common-web-programming)

  (make-local-variable 'web-mode-indentation-params)
  (make-local-variable 'web-mode-markup-indent-offset)
  (make-local-variable 'web-mode-css-indent-offset)
  (make-local-variable 'web-mode-code-indent-offset)
  (make-local-variable 'web-mode-indent-offset)

  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset    2
        web-mode-code-indent-offset   2)

  (local-set-key "."  '(lambda ()
                         (interactive)
                         (insert-char ?.)
                         (indent-for-tab-command)))

  (local-set-key "}"  '(lambda ()
                         (interactive)
                         (insert-char ?})
                         (indent-for-tab-command)))

  ;; Run hook after local variables loaded.
  (add-hook 'web-mode-local-vars-hook 'init/web/load-local-vars)

  ;; javascript: make case indentation at the same level as the parent switch
  ;; statement.
  (add-to-list 'web-mode-indentation-params '("case-extra-offset" . nil))

  ;; Disable `flycheck-mode' for buffers where the eslint checker does not
  ;; apply and would produce false positives.
  (let ((ext (file-name-extension (buffer-file-name))))
    (cond ((string-match "^\\(css\\|html\\|less\\)$" ext)
           (message "warn: flycheck-mode disabled")
           (flycheck-mode -1))))

  ;; Set content type to jsx for source files with "js" or "jsx" suffix.
  ;; Source: http://cha1tanya.com/2015/06/20/configuring-web-mode-with-jsx.html
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")))

  ;; Only use the company backends that we actually need.
  (when (boundp 'company-backend)
        (set (make-local-variable 'company-backends)
             '(company-flow company-yasnippet company-files))
        ; Was previously loading `company-tern' and tern too as given below:
        ;
        ; (set (make-local-variable 'company-backends)
        ;      '(company-flow company-tern company-yasnippet company-files))
        ; (tern-mode)
        )

  ;; Fix RET and C-j inverted behaviour
  (fix-indent-inverted-behaviour)

  ;; Note that we _must_ disable `flow-minor-mode' in the typescript-mode init sequence.
  (unless (condition-case nil
            (or typescript-mode tide-mode)
          (error nil))
    (flow-minor-mode 1)))

(defun init/web/load-local-vars ()
  "Map the value of `c-basic-offset' to `web-mode-code-indent-offset'."
  (when file-local-variables-alist
    (dolist (elt file-local-variables-alist)
      (let* ((var (car elt))
             (val (cdr elt)))
        (cond ((eq var 'c-basic-offset)
               (setq web-mode-code-indent-offset val))
              )))))

;; For better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
;;
;; Taken from: http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;;
;; NOTE: deactivated as it seems to create issues
;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
;;   (if (equal web-mode-content-type "jsx")
;;     (let ((web-mode-enable-part-face nil))
;;       ad-do-it)
;;     ad-do-it))

(use-package web-mode
  :mode ("\\.jsx?\\'" "\\.html?\\'")
  :hook ((web-mode . init/web-mode)))

;;; web.el ends here
