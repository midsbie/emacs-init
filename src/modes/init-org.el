;;; init-org.el --- Configure Org mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun my/org-kill-all-buffers ()
  "Kill all `org-mode'-related buffers."
  (interactive)
  (mapc #'kill-buffer
        (seq-filter (lambda (buf)
                      (with-current-buffer buf
                        (memq major-mode
                              '(org-mode org-agenda-mode calendar-mode))))
                    (buffer-list)))
  (message "Killed all Org-mode buffers"))

(defun init/org/enable ()
  "Enhance `org-mode' with visual and behavior tweaks."
  (org-indent-mode 1)
  (visual-line-mode 1)
  ;; Org-mode relies on a tab-width of 8 for correct table alignment and
  ;; proper functioning of certain navigation commands.
  (setq-local tab-width 8)
  (local-set-key (kbd "M-h") #'backward-kill-word))

(use-package org
  :ensure nil
  :hook (org-mode . init/org/enable)
  :bind
  (("C-c a a" . org-agenda-list)
   ("C-c a t" . org-todo-list)
   ("C-c c"   . org-capture)
   ("C-x K o" . my/org-kill-all-buffers))
  :custom
  (org-agenda-window-setup 'current-window)
  (org-clock-persist 'history)
  (org-confirm-babel-evaluate nil)
  (org-directory "~/documents/org")
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-startup-with-link-previews t)
  (org-use-fast-todo-selection 'auto)

  (org-todo-keywords
   '((sequence "TODO(t)" "IN-PROGRESS(p!)" "WAIT(w@/!)"
               "|"
               "DONE(d!)" "CANCELED(c@)")))
  (org-todo-keyword-faces
   '(("TODO" . org-todo)
     ("NEXT" . org-todo)
     ("IN-PROGRESS" . warning)
     ("DONE" . org-done)
     ("WAIT" . warning)
     ("WAITING" . warning)
     ("CANCELED" . org-done)
     ("CANCELLED" . org-done)
     ("SOMEDAY" . warning)
     ("OPEN" . org-todo)
     ("CLOSED" . org-done)
     ("ONGOING" . warning)))
  :config
  (org-clock-persistence-insinuate)
  (add-hook 'org-babel-after-execute-hook #'org-link-preview-region)
)

(provide 'init-org)

;;; init-org.el ends here
