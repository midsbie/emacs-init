;;; desktop.el --- Configures the desktop feature

;; Copyright (C) 2015-2023  Miguel Guedes

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

(defun init/desktop/update-minor-mode-table()
  (when (boundp 'loop)
    (cl-loop for mode in minor-mode-list do
             (add-to-list 'desktop-minor-mode-table (list mode nil)))))

(use-package desktop
  :defer t
  :hook (;; Set our preferred window arrangement after successfully loading a
         ;; desktop configuration.
         (desktop-after-read-hook . my/rearrange-desktop)
         ;; Add all known minor modes to `desktop-minor-mode-table' to prevent
         ;; `desktop-save' from saving the minor modes associated with each file.
         (find-file-hook . init/desktop/update-minor-mode-table)
         )
  :config
  (unless (boundp 'desktop-minor-mode-table)
    (setq desktop-minor-mode-table nil))

  ;; Make it so `desktop-change-dir' and `desktop-save' prompt for the current
  ;; working directory by default.
  (eval-after-load 'desktop-change-dir
    '(add-to-list 'desktop-path "."))
  )

;;; desktop.el ends here
