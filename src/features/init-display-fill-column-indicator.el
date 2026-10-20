;;; init-display-fill-column-indicator.el --- Configure display-fill-column-indicator -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configuration for the display-fill-column-indicator library.

;;; Code:

(defun init/display-fill-column-indicator/disable-in-org ()
  "Disable the fill-column indicator in Org buffers."
  (display-fill-column-indicator-mode -1))

(use-package display-fill-column-indicator
  :ensure nil
  :diminish
  ;; Opt-in rather than global: avoids the indicator in buffers where
  ;; fill-column is meaningless (terminals, special buffers).  conf-mode and
  ;; yaml-ts-mode don't derive from prog-mode, and org-mode derives from
  ;; text-mode but doesn't benefit from the indicator, so they need explicit
  ;; hooks/overrides.
  :hook (((prog-mode text-mode conf-mode yaml-ts-mode)
          . display-fill-column-indicator-mode)
         (org-mode . init/display-fill-column-indicator/disable-in-org)))

(provide 'init-display-fill-column-indicator)

;;; init-display-fill-column-indicator.el ends here
