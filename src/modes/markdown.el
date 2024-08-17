;;; markdown.el --- Configures markdown-mode.

;; Copyright (C) 2024  Miguel Guedes

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

(use-package markdown-mode
  :config
  ;; Reassign the key binding that conflicts with `project-prefix-map', then
  ;; expose `project' bindings.
  (define-key markdown-mode-map (kbd "C-c C-p") nil)
  (define-key markdown-mode-map (kbd "C-c M-p") 'markdown-outline-previous)

  (define-key markdown-mode-map (kbd "C-c p") project-prefix-map))

;;; markdown.el ends here
