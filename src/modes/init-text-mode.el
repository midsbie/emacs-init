;;; init-text-mode.el --- Configure text mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/text-mode/enable ()
  "Initialises `text-mode'."
  (when (eq major-mode 'text-mode)
    (auto-fill-mode 1))
  ;; The YAML modes derive from `text-mode' too; only check their comments and
  ;; strings, not keys and values.
  (if (derived-mode-p 'yaml-mode 'yaml-ts-mode)
      (flyspell-prog-mode)
    (flyspell-mode 1))
  (abbrev-mode -1)                  ; turn abbrev-mode off
  )

(use-package text-mode
  :ensure nil
  :hook (text-mode . init/text-mode/enable))

(provide 'init-text-mode)

;;; init-text-mode.el ends here
