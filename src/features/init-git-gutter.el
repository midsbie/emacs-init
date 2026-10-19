;;; init-git-gutter.el --- Configures the `git-gutter' package  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026  Miguel Guedes

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
;; Project hosted on Github: https://github.com/emacsorphanage/git-gutter
;;
;; Fringe rendering is configured in init-git-gutter-fringe.el.

;;; Code:

(use-package git-gutter
  :demand t
  :diminish
  :bind (("C-x C-g !" . git-gutter)
         ("C-x C-g =" . git-gutter:popup-hunk)
         ("C-x C-g p" . git-gutter:previous-hunk)
         ("C-x C-g n" . git-gutter:next-hunk)
         ("C-x C-g s" . git-gutter:stage-hunk)
         ("C-x C-g r" . git-gutter:revert-hunk)
         ("C-x C-g SPC" . git-gutter:mark-hunk))
  :custom
  (git-gutter:update-interval 2)
  (git-gutter:lighter "")
  (git-gutter:hide-gutter t)
  (git-gutter:modified-sign "*")
  :config
  (require 'git-gutter-fringe)
  (global-git-gutter-mode 1))

(provide 'init-git-gutter)

;;; init-git-gutter.el ends here
