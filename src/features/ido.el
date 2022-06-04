;;; ido.el --- Configures ido and related packages

;; Copyright (C) 2015-2022  Miguel Guedes

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

(defun init/ido ()
  "Initialize the `ido' package."

  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching  t
        ido-use-faces             nil))

(defun init/config/ido ()
  (ido-mode 1)
  (ido-everywhere 1))

(use-package ido
  :init (init/ido)
  :config (init/config/ido))

(use-package flx-ido
  :after ido
  :config (flx-ido-mode 1))

;;; ido.el ends here
