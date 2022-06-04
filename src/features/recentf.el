;;; recentf.el --- Configures `recentf' package

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

(defun init/recentf ()
  "Initialises the `recentf' package."

  (setq recentf-auto-cleanup    'never
        recentf-max-menu-items  100)

  ;; Save files every 2 minutes, suppressing the message printed in the
  ;; minibuffer.
  ;; Ref: https://emacs.stackexchange.com/questions/14706/
  (run-at-time nil (* 2 60) '(lambda ()
                               (let (message-log-max)
                                 (recentf-save-list)))))

(use-package recentf
  ;; The following replaces default binding to `ido-find-file-read-only'
  ;; :bind ("C-x C-r" . recentf-open-files)
  :bind ("C-x C-r" . consult-recent-file)
  :init (init/recentf)
  :config (recentf-mode 1))

;;; recentf.el ends here
