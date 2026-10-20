;;; my-warn-when-root.el --- Mode line warnings for root buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:


;;;###autoload
(defun my/warn-when-root-visits-file ()
  "Warn when visiting a buffer whose default directory belongs to root.
Displays the mode line with a red background when the effective
user is root via TRAMP (sudo, su, doas, or multi-hop)."
  (when (and (not (bound-and-true-p my/--root-warning-applied))
             (file-remote-p default-directory)
             (string= "root" (file-remote-p default-directory 'user)))
    (face-remap-add-relative
     'mode-line-active
     '(:background "red3" :foreground "white" :box nil))
    (face-remap-add-relative
     'mode-line-inactive
     '(:background "#4a0000" :foreground "gray40" :box nil))
    (setq-local my/--root-warning-applied t)))

(provide 'my-warn-when-root)

;;; my-warn-when-root.el ends here
