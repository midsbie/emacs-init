;;; init-php-ts-mode.el --- Configure PHP modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/php-ts-mode/enable ()
  "Configure `php-mode'."
  (init/common-programming-mode)

  (setq-local comment-start "/* ")
  (setq-local comment-end   " */"))

(use-package php-ts-mode
  :ensure nil
  :mode "\\.php\\'"
  :hook (php-ts-mode . init/php-ts-mode/enable))

(provide 'init-php-ts-mode)

;;; init-php-ts-mode.el ends here
