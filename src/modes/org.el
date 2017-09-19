;;; org.el --- Configures `org-mode'

;; Copyright (C) 2015  Miguel Guedes

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


(eval-after-load 'org
  '(progn
     (setq org-use-fast-todo-selection t
           org-directory               "~/documents/org"
           org-default-notes-file      (concat org-directory "/notes.org"))

     (setq org-todo-keyword-faces
           '(("TODO"  . (:foreground "red" :weight bold))
             ("NEXT"  . (:foreground "red" :weight bold))
             ("DONE"  . (:foreground "forest green" :weight bold))
             ("WAITING"  . (:foreground "orange" :weight bold))
             ("CANCELLED"  . (:foreground "forest green" :weight bold))
             ("SOMEDAY"  . (:foreground "orange" :weight bold))
             ("OPEN"  . (:foreground "red" :weight bold))
             ("CLOSED"  . (:foreground "forest green" :weight bold))
             ("ONGOING"  . (:foreground "orange" :weight bold))))

     (setq org-clock-persist 'history)
     (org-clock-persistence-insinuate)

     (setq org-todo-keywords
           '((sequence "TODO(t)" "IN-PROGRESS(p!)" "WAIT(w@/!)"
                       "|"
                       "DONE(d!)" "CANCELED(c@)")))

     ;; (setq org-todo-keywords
     ;;       '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
     ;;         (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")
     ;;         (sequence "SOMEDAY(s!/!)" "|")
     ;;         (sequence "OPEN(O!)" "|" "CLOSED(C!)")
     ;;         (sequence "ONGOING(o!/!)" "|")))

     (add-hook 'org-mode-hook 'init-org)

     (global-set-key (kbd "C-c a a")   'org-agenda-list)
     (global-set-key (kbd "C-c a t")   'org-todo-list)
     (global-set-key (kbd "C-c c")     'org-capture)
     (global-set-key (kbd "C-x K o")   'org-kill-all-buffers)

     (custom-set-variables
      '(org-agenda-window-setup (quote current-window)))))

(defun init-org ()
  ;; Turn on indent and visual line modes by default
  (org-indent-mode 1)
  (visual-line-mode)
  (auto-fill-mode -1)

  ;; Remaps
  (local-set-key (kbd "M-h") 'backward-kill-word))

(defun org-kill-all-buffers ()
  "Kill all `org-mode' buffers."
  (interactive)
  (mapc 'kill-buffer
        (remove-if-not #'(lambda(buffer)
                           (with-current-buffer buffer
                             (or
                              (string= major-mode "calendar-mode")
                              (string= major-mode "org-mode")
                              (string= major-mode "org-agenda-mode"))))
                       (buffer-list)))
  (when (called-interactively-p 'interactive)
    (message "Killed all org-mode buffers")))

;;; org.el ends here
