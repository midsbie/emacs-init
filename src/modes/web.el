;;; web.el --- Configures `web-mode'

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

(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.react.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(eval-after-load 'web-mode
  '(progn
     (add-hook 'web-mode-hook  'init-web-mode)
     (add-hook 'web-mode-hook  'init-common-web)))

(defun init-web-mode ()
  "Initialise `web-mode'."
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

  ;; Disable `flycheck-mode' for buffers where the eslint checker does not
  ;; apply and would produce false positives.
  (let ((ext (file-name-extension (buffer-file-name))))
    (cond ((string-match "^\\(css\\|html\\|less\\)$" ext)
           (message "warn: flycheck-mode disabled")
           (flycheck-mode -1))))
  )

;;; web.el ends here
