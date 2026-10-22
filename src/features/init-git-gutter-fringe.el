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
  :config (init/git-gutter-fringe/config))

(provide 'init-git-gutter-fringe)

;;; init-git-gutter-fringe.el ends here
