;;; codesearch.el --- Customisations for the `codesearch' package

;; Copyright (C) 2017-2018  Miguel Guedes

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

;; NOTE: the codesearch package does not seem to work AT ALL if the
;; byte-compiled binaries are present in its installation directory.  A cryptic
;; error was being thrown each time any of the functions was executed, which
;; for future reference was: "Symbol's value as variable is void: G661".  The
;; solution was to delete all ".elc" files in the codesearch directory in the
;; emacs-elpa repository.
(global-set-key (kbd "C-c s g") 'listing-codesearch-search)
(global-set-key (kbd "C-c s l") 'listing-codesearch-list-directories)
(global-set-key (kbd "C-c s u") 'init/codesearch-update-index)

(defun init/codesearch-update-index()
  (interactive)
  (codesearch-update-index)
  (message "Codesearch index updated"))

;;; codesearch.el ends here
