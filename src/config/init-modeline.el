;;; init-modeline.el --- Mode line display and root warnings  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(my/modeline-mode 1)
(add-hook 'after-change-major-mode-hook #'my/warn-when-root-visits-file)

(provide 'init-modeline)

;;; init-modeline.el ends here
