;;; magit.el --- Configures the magit feature

;; Copyright (C) 2015  Miguel Guedes

;; Author: Miguel Guedes <miguel@miguelguedes.org>
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


(global-set-key (kbd "C-c C-g") '(run-or-load 'magit 'magit-status))

(eval-after-load 'magit
  '(progn
     (global-set-key (kbd "C-c C-g") 'magit-status)
     (add-to-list 'git-commit-mode-hook 'init-magit-commit-mode)

     (defun init-magit-commit-mode ()
       (set-fill-column 72))))


;;; magit.el ends here
