;;; init-speedbar.el --- Configure speedbar  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package speedbar
  :ensure nil
  :defer t
  :custom
  (speedbar-frame-parameters '((minibuffer . nil)
                              (width . 40)
                              (border-width . 0)
                              (menu-bar-lines . 0)
                              (tool-bar-lines . 0)
                              (unsplittable . t)
                              (left-fringe . 0))))

(provide 'init-speedbar)

;;; init-speedbar.el ends here
