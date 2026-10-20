;;; init-window-pulse.el --- Configure window-pulse  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package window-pulse
  :vc (:url "https://github.com/midsbie/window-pulse"
            :rev :newest)
  :config (window-pulse-mode 1))

(provide 'init-window-pulse)

;;; init-window-pulse.el ends here
