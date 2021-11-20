;;; tide.el --- Configures TIDE

;; Copyright (C) 2021  Miguel Guedes

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

(defun init/tide ()
  "Initialise the TIDE package."
  ;; For some reason, 'company-tide is either not being added to the list of
  ;; company backends or it is somehow being removed, perhaps by `lsp-mode'.
  ;; It is explicitly added here to ensure that completions work in TIDE.
  (cl-pushnew 'company-tide company-backends))

(use-package tide
  :after (company)
  :diminish "TIDE"
  :bind ((:map tide-mode-map
               ("C-c t s" . tide-restart-server)
               ("C-c t S" . tide-kill-server)
               ("C-c t p" . tide-project-errors)
               ("C-c C-c" . tide-fix)
               ("C-c t x" . tide-references)
               ("C-c t m" . tide-rename-symbol)
               ("C-c t M" . tide-rename-file)
               ("C-c t r" . tide-refactor)))
  :custom
  (tide-server-max-response-length 1024000) ; x10 the default value
  :init (init/tide))

;;; tide.el ends here
