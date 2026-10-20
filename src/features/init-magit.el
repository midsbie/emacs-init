;;; init-magit.el --- Configure magit  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

;; Declared special so the `let' below binds it dynamically even before magit
;; has loaded.
(defvar magit-display-buffer-function)

(defun init/magit/display-buffer (buffer)
  "Display BUFFER, reusing visible diff and process buffers.
Open hidden diff and process buffers in another window.  Display other
Magit buffers in the selected window."
  (display-buffer
   buffer (if (with-current-buffer buffer
                (derived-mode-p 'magit-diff-mode 'magit-process-mode))
              (if (get-buffer-window buffer)
                  '(display-buffer-reuse-window)
                '(nil (inhibit-same-window . t)))
            '(display-buffer-same-window))))

(defun my/magit/status-other-window ()
  "Show magit status in the other window."
  (interactive)
  (let ((magit-display-buffer-function #'magit-display-buffer-traditional))
    (magit-status)))

(defun my/magit/preview-file ()
  "Show the file or blob at point in another window, retaining focus."
  (interactive)
  (save-selected-window
    (magit-diff-visit-file t)))

(use-package magit
  :custom
  ;; Don't ask to save buffers
  (magit-save-repository-buffers nil)
  (magit-display-buffer-function #'init/magit/display-buffer)
  ;; Enable word-level diff highlighting in all hunks.
  (magit-diff-refine-hunk 'all)

  :bind (("C-x g"   . magit-status)
         ("C-x 4 g" . my/magit/status-other-window)
         :map magit-diff-section-map
         ("SPC" . my/magit/preview-file)))

(provide 'init-magit)

;;; init-magit.el ends here
