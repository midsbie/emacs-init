;;; init-neotree.el --- Customises the neotree packge  -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar my/neotree/refresh-timer nil)
(defvar my/neotree/last-buffer nil)

(defun my/neotree/refresh-on-buffer-change ()
  "Refresh neotree when switching buffers.

This function records the last refreshed buffer to prevent unnecessary
updates from taking place."
  (interactive)
  (when (and (neo-global--window-exists-p)
             (buffer-file-name)
             (not (eq my/neotree/last-buffer (current-buffer))))
    (setq my/neotree/last-buffer (current-buffer))
    (when my/neotree/refresh-timer
      (cancel-timer my/neotree/refresh-timer))
    (setq my/neotree/refresh-timer
          (run-with-idle-timer .25 nil #'(lambda ()
                                          (neotree-refresh t))))))

(use-package neotree
  ;; The buffer-list hook calls Neotree internals, so load them before wiring it.
  :demand t
  :custom
  (neo-window-width 50)
  (neo-smart-open t)
  (neo-theme 'icons)
  :hook ((buffer-list-update . my/neotree/refresh-on-buffer-change)))

(provide 'init-neotree)

;;; init-neotree.el ends here
