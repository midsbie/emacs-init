;;; debug-init.el --- Activate packages for debugging  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Apply early-init.el and activate installed packages without loading init.el.
;; Usage: emacs -Q -l /path/to/src/debug-init.el

;;; Code:

;; -Q skips both early-init.el and package activation.
(load (expand-file-name "early-init"
                        (file-name-directory (file-truename load-file-name)))
      nil 'nomessage)
(package-activate-all)
(setopt use-package-always-ensure t)

;;; debug-init.el ends here
