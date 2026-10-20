;;; init-project.el --- Configure project  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

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

(use-package project
  :ensure nil
  ;; Don't use C-c C-p as it clashes with many major mode default bindings.
  :bind-keymap ("C-c P" . project-prefix-map)
  :bind (:map project-prefix-map
              ("4" . project-other-window-command)
              ("/" . my/project-bury-buffers)
              ("m" . magit-project-status))
  :custom
  ;; Menu shown by `project-switch-project' once a project is chosen.  Each
  ;; command's key is looked up in `project-prefix-map'.  Magit replaces VC-Dir.
  (project-switch-commands
   '((project-find-file "Find file")
     (project-find-regexp "Find regexp")
     (project-find-dir "Find directory")
     (project-dired "Dired")
     (magit-project-status "Magit")
     (project-eshell "Eshell")
     (project-any-command "Other"))))

(provide 'init-project)

;;; init-project.el ends here
