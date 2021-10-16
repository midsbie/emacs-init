;;; git-gutter.el --- Configures the `git-gutter' package

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
;; Project hosted on Github: https://github.com/emacsorphanage/git-gutter

;;; Code:

(defun init/git-gutter ()
  "Initialise git-gutter."
  (global-git-gutter-mode t)

  (let ((prefix "C-x C-g "))
    (global-set-key (kbd (concat prefix "!")) #'git-gutter)
    (global-set-key (kbd (concat prefix "=")) #'git-gutter:popup-hunk)

    ;; Jump to next/previous hunk
    (global-set-key (kbd (concat prefix "p")) #'git-gutter:previous-hunk)
    (global-set-key (kbd (concat prefix "n")) #'git-gutter:next-hunk)

    ;; Stage current hunk
    (global-set-key (kbd (concat prefix "s")) #'git-gutter:stage-hunk)

    ;; Revert current hunk
    (global-set-key (kbd (concat prefix "r")) #'git-gutter:revert-hunk)

    ;; Mark current hunk
    (global-set-key (kbd (concat prefix "SPC")) #'git-gutter:mark-hunk))

  (setq git-gutter:modified-sign " "
        git-gutter:added-sign " "
        git-gutter:deleted-sign " "
        git-gutter:update-interval 2
        git-gutter:lighter ""
        git-gutter:hide-gutter t)

  (set-face-background 'git-gutter:modified "dark magenta")
  (set-face-attribute 'git-gutter:modified nil :height 60)
  (set-face-background 'git-gutter:added "dark green")
  ;; Looks wider than modified hunks if set to 60 for some strange reason
  (set-face-attribute 'git-gutter:added nil :height 50)
  (set-face-background 'git-gutter:deleted "dark red")
  (set-face-attribute 'git-gutter:deleted nil :height 60))

(use-package git-gutter
  :ensure t
  :init
  (init/git-gutter)
  :config)

;;; git-gutter.el ends here
