;;; ido.el --- Configures ido and related packages

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

;; Configuration of IDO is disabled because it conflicts with the `consult'
;; package; note :if clause in use-package declaration.  The conflict occurs
;; when opening a NEW file whose name partially matches that of an existing
;; file.  When this happens, it is not possible to hit M-RET to instruct consult
;; to accept the entered name as-is.

;;; Code:

(defun init/ido/config ()
  "Initialize the `ido' package."

  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)

  Disable ido faces to see flx highlights.
  Ref: https://github.com/lewang/flx
  (setq ido-enable-flex-matching  t
        ido-use-faces             nil))

(use-package flx-ido
  :if (not (featurep 'consult))
  :after ido
  :config (init/ido/config))

;;; ido.el ends here
