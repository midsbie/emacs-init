;;; magit.el --- Configures the magit feature

;; Copyright (C) 2015-2026  Miguel Guedes

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

(defun my/magit/status-other-window ()
  "Show magit status in the other window."
  (interactive)
  (let ((magit-display-buffer-function #'magit-display-buffer-traditional))
    (magit-status)))

(use-package magit-status
  :custom
  ;; Don't ask to save buffers
  (magit-save-repository-buffers nil)
  ;; Always display magit buffers in the current window.
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  :bind (("C-x g"   . magit-status)
         ("C-x 4 g" . my/magit/status-other-window)))

;;; magit.el ends here
