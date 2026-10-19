;;; init-css-mode.el --- Configures `css-mode'  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026  Miguel Guedes

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

(defun init/css-mode/enable ()
  "Configure `css-mode'."
  (init/common-nonweb-programming-mode)

  (local-set-key "}"  #'(lambda ()
                          (interactive)
                          (insert-char ?})
                          (indent-for-tab-command)))

  (local-set-key ";"  #'(lambda ()
                          (interactive)
                          (insert-char ?\;)
                          (indent-for-tab-command))))

(use-package css-mode
  :ensure nil
  :mode (("\\.css\\'" . css-ts-mode))
  :custom
  (css-indent-offset 2)
  ;; SCSS inherits css-mode-hook; configuring it again would start LSP twice.
  :hook ((css-mode css-ts-mode) . init/css-mode/enable))

(provide 'init-css-mode)

;;; init-css-mode.el ends here
