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
;;
;; A separate configuration for `git-gutter-fringe' is not provided because the
;; interface is identical to `git-gutter' and it automatically defaults to the
;; latter if not running in a graphical environment.

;;; Code:

(defun init/git-gutter ()
  "Initialise git-gutter."
  ;; The form `(use-package 'git-gutter-fringer)` did not work, but this did:
  (require 'git-gutter-fringe)

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

  (dolist (sym '(git-gutter-fr:added git-gutter-fr:modified git-gutter-fr:deleted))
    (fringe-helper-define sym nil
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"
      "XX......"))

  (setq git-gutter:update-interval 2
        git-gutter:lighter ""
        git-gutter:hide-gutter t)

  (run-with-timer 2 nil #'(lambda ()
                            (let* ((fringe-bg (face-attribute 'fringe :background)))
                              (set-face-foreground 'git-gutter-fr:modified "SkyBlue3")
                              (set-face-background 'git-gutter-fr:modified fringe-bg)
                              (set-face-foreground 'git-gutter-fr:added "dark green")
                              (set-face-background 'git-gutter-fr:added fringe-bg)
                              (set-face-foreground 'git-gutter-fr:deleted "dark red")
                              (set-face-background 'git-gutter-fr:deleted fringe-bg)

                              (set-face-background 'git-gutter:modified "SkyBlue3")
                              (set-face-background 'git-gutter:added "dark green")
                              (set-face-background 'git-gutter:deleted "dark red"))))

  (setq git-gutter:modified-sign "*")
  (global-git-gutter-mode t))

(use-package git-gutter
  :diminish
  :init
  (init/git-gutter)
  :config)

;;; git-gutter.el ends here
