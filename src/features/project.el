;;; project.el --- Configures the Emacs `project' package  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Miguel Guedes

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun my/project-bury-buffers ()
  "Bury all buffers belonging to the current project in every window of the frame.
Each window is cycled until it displays a non-project buffer or all buffers have
been tried."
  (interactive)
  (let ((pr (project-current)))
    (if (not pr)
        (message "Current buffer is not part of a project")
      (let ((project-bufs (project-buffers pr)))
        (dolist (win (window-list))
          (let ((attempts 0)
                (limit (length (buffer-list))))
            (while (and (memq (window-buffer win) project-bufs)
                        (< attempts limit))
              (with-selected-window win
                (bury-buffer))
              (setq attempts (1+ attempts)))))))))

(defun my/project-find-file-other-window (&optional include-all)
  "Open FILENAME from a project in another window."
  (interactive)
  (unwind-protect
      (progn
        (advice-add 'find-file :override #'find-file-other-window)
        (project-find-file include-all))
    (advice-remove 'find-file #'find-file-other-window)))

(use-package project
  ;; Don't use C-c C-p as it clashes with many major mode default bindings.
  :bind-keymap ("C-c P" . project-prefix-map)
  :bind (:map project-prefix-map
              ("4 f" . my/project-find-file-other-window)
              ("/" . my/project-bury-buffers)))

;;; project.el ends here
