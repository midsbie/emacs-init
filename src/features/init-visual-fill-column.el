;;; init-visual-fill-column.el --- Wrap visual lines at `fill-column'  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: convenience

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

;; Wrap at each buffer's `fill-column' when selected with C-S-w.
;; Plain `visual-line-mode' continues to wrap at the window width.

;;; Code:

(use-package visual-fill-column
  :custom
  (visual-fill-column-width nil)
  :config
  ;; Remove the previous coupling when this configuration is reloaded.
  (remove-hook 'visual-line-mode-hook #'visual-fill-column-for-vline)
  ;; Recalculate the margins after changing the column or zooming the text.
  (advice-add 'set-fill-column :after #'visual-fill-column-adjust)
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

(provide 'init-visual-fill-column)

;;; init-visual-fill-column.el ends here
