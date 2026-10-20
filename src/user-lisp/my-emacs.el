;;; my-emacs.el --- Emacs utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

;;;###autoload
(defun my/clear-before-save-hooks()
  "Clear all `before-save-hook' lists in all buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (progn
      (with-current-buffer buf
        (setq before-save-hook nil)))))

(provide 'my-emacs)

;;; my-emacs.el ends here
