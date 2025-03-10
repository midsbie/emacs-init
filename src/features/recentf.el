;;; recentf.el --- Configures `recentf' package

;; Copyright (C) 2015-2024  Miguel Guedes

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

(use-package recentf
  :demand
  ;; The following replaces default binding to `ido-find-file-read-only'
  ;; :bind ("C-x C-r" . recentf-open-files)

  :custom
  (recentf-auto-cleanup    'never)
  (recentf-max-menu-items  100)

  :config
  (recentf-mode 1)
  )

;;; recentf.el ends here
