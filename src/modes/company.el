;;; company.el --- Configures `company-mode'

;; Copyright (C) 2017-2020  Miguel Guedes

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

(defun init/company()
  ;; Note that there is no need to require the libraries for the following
  ;; minor modes.
  (global-company-mode)
  (company-statistics-mode)

  ;; decrease delay before autocompletion popup shows
  (setq company-idle-delay .3)
  ;; start autocompletion only after typing
  (setq company-begin-commands '(self-insert-command))
  ;; Avoid annoying suggestion of ".." when just cd'ing into a directory in
  ;; shell mode.
  (setq company-files-exclusions (append company-files-exclusions '("." "..")))

  (add-to-list 'company-backends 'company-flow)
  (define-key company-mode-map (kbd "<C-return>") 'company-complete))

(use-package company
  :after prog-mode
  :config
  (init/company))

;;; company.el ends here
