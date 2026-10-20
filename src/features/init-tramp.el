;;; init-tramp.el --- Configure tramp  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/tramp/config ()
  "Configure the `tramp' package."

  ; Invoke by opening /sudo:sudo-zeus:/path/to/file
  (add-to-list 'tramp-default-proxies-alist
               '("sudo-zeus" nil "/ssh:softgeist.com:")))

;; Moved to after the function declaration so as to avoid potential failures at
;; load time
(use-package tramp
  :ensure nil
  :defer t
  :config (init/tramp/config))

(provide 'init-tramp)

;;; init-tramp.el ends here
