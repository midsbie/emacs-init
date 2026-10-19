;;; init-git-gutter-fringe.el --- Configure git-gutter-fringe -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configuration for the git-gutter-fringe library.

;;; Code:

(defun init/git-gutter-fringe/config ()
  "Configure Git Gutter's fringe bitmaps and colors."
  (dolist (sym '(git-gutter-fr:added git-gutter-fr:modified git-gutter-fr:deleted))
    (fringe-helper-define sym nil
      "....XXXX"
      "....XXXX"
      "....XXXX"
      "....XXXX"
      "....XXXX"
      "....XXXX"
      "....XXXX"
      "....XXXX"
      "....XXXX"
      "....XXXX"
      "....XXXX"
      "....XXXX"
      "....XXXX"
      "....XXXX"
      "....XXXX"
      "....XXXX"))

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
                              (set-face-background 'git-gutter:deleted "dark red")))))

(use-package git-gutter-fringe
  :defer t
  :config (init/git-gutter-fringe/config))

(provide 'init-git-gutter-fringe)

;;; init-git-gutter-fringe.el ends here
