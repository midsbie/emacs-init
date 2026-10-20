;;; init-consts.el --- Shared constants and aliases  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defgroup init nil
  "Emacs initialisation mechanism.")

(defconst init/defaults/fill-column 100
  "Default `fill-column' value.")

(defconst init/defaults/fill-column/narrow 80
  "Default narrow `fill-column' value.")

(defalias 'hscroll-mode 'toggle-truncate-lines)

;; Alias query functions so all the replace functions start with "replace"!
(defalias 'replace-query 'query-replace)
(defalias 'replace-query-regexp 'query-replace-regexp)
(defalias 'replace-query-regexp-eval 'query-replace-regexp-eval)

(provide 'init-consts)

;;; init-consts.el ends here
