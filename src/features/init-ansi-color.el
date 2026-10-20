;;; init-ansi-color.el --- Configure ansi-color  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun my/colorize-buffer ()
  "Colorize the entirety of the current buffer if not read-only."
  (interactive)
  (if buffer-read-only
      (message "Unable to colorize read-only buffer")
    (ansi-color-apply-on-region (point-min) (point-max))))

(use-package ansi-color
  :ensure nil
  :hook ((compilation-filter . ansi-color-compilation-filter)))

(provide 'init-ansi-color)

;;; init-ansi-color.el ends here
