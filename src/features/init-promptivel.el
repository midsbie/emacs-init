;;; init-promptivel.el --- Configure promptivel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package promptivel
  :vc (:url "https://github.com/midsbie/promptivel"
            :rev :newest)
  :diminish promptivel-global-mode
  :config
  (promptivel-global-mode))

(provide 'init-promptivel)

;;; init-promptivel.el ends here
