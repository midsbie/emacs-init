;;; org.el --- Configuration for the `org' package

;; Copyright (C) 2015-2025  Miguel Guedes

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

(defun init/org-mode/enable ()
  "Enhance `org-mode' with visual and behavior tweaks."
  (org-indent-mode 1)
  (visual-line-mode 1)
  (auto-fill-mode -1)
  (display-fill-column-indicator-mode -1)
  (local-set-key (kbd "M-h") #'backward-kill-word))

(use-package org
  :hook (org-mode . init/org-mode/enable)
  :after speedbar
  :bind
  (("C-c a a" . org-agenda-list)
   ("C-c a t" . org-todo-list)
   ("C-c c"   . org-capture)
   ("C-x K o" . my/org-kill-all-buffers))
  :custom-face
  (org-todo ((t (:foreground "red" :weight bold))))
  (org-done ((t (:foreground "forest green" :weight bold))))
  :custom
  (org-agenda-window-setup 'current-window)
  (org-clock-persist 'history)
  (org-confirm-babel-evaluate nil)
  (org-directory "~/documents/org")
  (org-startup-with-inline-images t)
  (org-use-fast-todo-selection t)

  (org-todo-keywords
   '((sequence "TODO(t)" "IN-PROGRESS(p!)" "WAIT(w@/!)"
               "|"
               "DONE(d!)" "CANCELED(c@)")))
  (org-todo-keyword-faces
   '(("TODO" . (:foreground "red" :weight bold))
     ("NEXT" . (:foreground "red" :weight bold))
     ("DONE" . (:foreground "forest green" :weight bold))
     ("WAITING" . (:foreground "orange" :weight bold))
     ("CANCELLED" . (:foreground "forest green" :weight bold))
     ("SOMEDAY" . (:foreground "orange" :weight bold))
     ("OPEN" . (:foreground "red" :weight bold))
     ("CLOSED" . (:foreground "forest green" :weight bold))
     ("ONGOING" . (:foreground "orange" :weight bold))))
  :config
  (org-clock-persistence-insinuate)
  (setq org-default-notes-file (concat org-directory "/notes.org"))
)


;;; org.el ends here
