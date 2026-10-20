;;; init-sqlformat.el --- Configure sqlformat  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package sqlformat
  :custom
  (sqlformat-command 'pgformatter)
  (sqlformat-args '("-s2" "-g" "-w80")))

(provide 'init-sqlformat)

;;; init-sqlformat.el ends here
