;;; css.el --- Configures `css-mode'

;; Copyright (C) 2015-2023  Miguel Guedes

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

(defun init/css ()
  "Initialize `css-mode'."
  (css-eldoc-enable))

(defun init/css/mode ()
  "Configure `css-mode'."
  (setq-default css-indent-offset            2)
  (auto-fill-mode -1)
  (eldoc-mode 1)

  (local-set-key "}"  #'(lambda ()
                          (interactive)
                          (insert-char ?})
                          (indent-for-tab-command)))

  (local-set-key ";"  #'(lambda ()
                          (interactive)
                          (insert-char ?\;)
                          (indent-for-tab-command))))

(use-package css-mode
  :defer t
  :hook ((css-mode . init/common-nonweb-programming-mode)
         (css-mode . init/css/mode))
  :config
  (init/css))

;;; css.el ends here
