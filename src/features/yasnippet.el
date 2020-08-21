;;; yasnippet.el --- Configures the yasnippet feature

;; Copyright (C) 2015-2020  Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools, internal

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

(defun yas/c++/get-header-extension ()
  "Guess appropriate extension for header file.
Assumes currently open file is a C++ implementation source file."
  (let ((ext (file-name-extension (buffer-file-name))))
    (cond
     ((string= ext "cxx") "hxx")
     ((string= ext "C") "H")
     (t "h"))))

(defun yas/get-kill-ring-word (default-label)
  "Return last entry in kill ring if word or DEFAULT-LABEL."
  (get-kill-ring-word default-label))

(defun init/yasnippet()
  (yas/global-mode 1)

  (define-key yas-minor-mode-map (kbd "C-x y") 'yas/visit-snippet-file)

  ;; Strangely, just redefining one of the variations below won't work.
  (define-key yas-minor-mode-map [(tab)]        nil)
  (define-key yas-minor-mode-map (kbd "TAB")    nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil))

(use-package yasnippet
  :bind ([backtab] . yas-expand)
  :config
  (init/yasnippet))

;;; yasnippet.el ends here
