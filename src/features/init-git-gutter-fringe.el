;;; init-git-gutter-fringe.el --- Configure git-gutter-fringe  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/git-gutter-fringe/config ()
  "Configure Git Gutter's fringe bitmaps."
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
      "....XXXX")))

(use-package git-gutter-fringe
  :defer t
  :custom-face
  (git-gutter-fr:modified ((t (:inherit fringe :foreground "SkyBlue3"))))
  (git-gutter-fr:added ((t (:inherit fringe :foreground "dark green"))))
  (git-gutter-fr:deleted ((t (:inherit fringe :foreground "dark red"))))
  (git-gutter:modified
   ((t (:inherit default :weight bold :foreground "magenta" :background "SkyBlue3"))))
  (git-gutter:added
   ((t (:inherit default :weight bold :foreground "green" :background "dark green"))))
  (git-gutter:deleted
   ((t (:inherit default :weight bold :foreground "red" :background "dark red"))))
  :config (init/git-gutter-fringe/config))

(provide 'init-git-gutter-fringe)

;;; init-git-gutter-fringe.el ends here
