;;; init-sql.el --- Configure SQL modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/sql/enable ()
  (init/common-programming-mode))

(use-package sql
  :ensure nil
  :hook (sql-mode . init/sql/enable))

(provide 'init-sql)

;;; init-sql.el ends here
