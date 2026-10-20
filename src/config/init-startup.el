;;; init-startup.el --- Startup diagnostics -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Report startup diagnostics after initialization.

;;; Code:

(defun init/post-init ()
  "Report missing native compilation support and schedule startup timing."
  (unless (and (fboundp 'native-comp-available-p)
               (native-comp-available-p))
    (message "Native compilation is NOT available"))
  (run-with-idle-timer
   1 nil (lambda () (message "init took %s" (emacs-init-time)))))

(add-hook 'after-init-hook #'init/post-init)

(provide 'init-startup)

;;; init-startup.el ends here
