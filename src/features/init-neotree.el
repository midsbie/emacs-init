;;; init-neotree.el --- Configure neotree  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defvar my/neotree/refresh-timer nil)
(defvar my/neotree/last-buffer nil)

(defun my/neotree/refresh-on-buffer-change ()
  "Refresh neotree when switching buffers.

This function records the last refreshed buffer to prevent unnecessary
updates from taking place."
  (interactive)
  (when (and (get-buffer-window neo-buffer-name)
             (buffer-file-name)
             (not (eq my/neotree/last-buffer (current-buffer))))
    (setq my/neotree/last-buffer (current-buffer))
    (when my/neotree/refresh-timer
      (cancel-timer my/neotree/refresh-timer))
    (setq my/neotree/refresh-timer
          (run-with-idle-timer .25 nil #'neotree-refresh t))))

(use-package neotree
  ;; Load the buffer-name constant before registering the buffer-list hook.
  :demand t
  :custom
  (neo-window-width 50)
  (neo-smart-open t)
  (neo-theme 'icons)
  :hook ((buffer-list-update . my/neotree/refresh-on-buffer-change)))

(provide 'init-neotree)

;;; init-neotree.el ends here
