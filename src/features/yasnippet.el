;;; yasnippet.el --- Configures the yasnippet feature

;; Copyright (C) 2015-2018  Miguel Guedes

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

(global-set-key [backtab] (t-run-or-load 'yas-expand 'yasnippet))

(eval-after-load 'yasnippet
  '(progn
     ;; Unset the global key defined above.
     (global-set-key [backtab] nil)

     (yas/global-mode 1)

     (define-key yas-minor-mode-map (kbd "C-x y") 'yas/visit-snippet-file)
     (define-key yas-minor-mode-map [backtab]     'yas-expand)

     ;; Strangely, just redefining one of the variations below won't work.
     (define-key yas-minor-mode-map [(tab)]        nil)
     (define-key yas-minor-mode-map (kbd "TAB")    nil)
     (define-key yas-minor-mode-map (kbd "<tab>") nil)))

(defun yas/c++/get-header-extension ()
  "Guess appropriate extension for header file.
Assumes currently open file is a C++ implementation source file."
  (let ((ext (file-name-extension (buffer-file-name))))
    (cond
     ((string= ext "cxx") "hxx")
     ((string= ext "C") "H")
     (t "h"))))

;;; yasnippet.el ends here
