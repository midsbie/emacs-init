;;; init-css-eldoc.el --- Configure css-eldoc -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configuration for the css-eldoc library.

;;; Code:

(defun init/css-eldoc/documentation (callback &rest _)
  "Pass CSS documentation at point to Eldoc's CALLBACK."
  (when-let* ((documentation (css-eldoc-function)))
    (funcall callback documentation)
    t))

(defun init/css-eldoc/enable ()
  "Add CSS documentation to Eldoc in the current buffer."
  (add-hook 'eldoc-documentation-functions #'init/css-eldoc/documentation nil t)
  (eldoc-mode 1))

(use-package css-eldoc
  :hook ((css-mode css-ts-mode) . init/css-eldoc/enable))

(provide 'init-css-eldoc)

;;; init-css-eldoc.el ends here
