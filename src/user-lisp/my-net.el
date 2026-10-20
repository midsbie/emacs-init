;;; my-net.el --- Web and networking utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

;; The following meant to be exposed as a command; do not prefix the function name.
;;;###autoload
(defun my/google(query)
  "Open Google search with QUERY as parameter.
Prompts the user for a query string, if not provided, that is
constructed as a URL that causes the current browser provider to
run and navigate to the Google search page showing results for
the specified query string."
  (interactive "sQuery string: ")
  (browse-url (concat "https://google.com/search?q=" (url-hexify-string query))))

(provide 'my-net)

;;; my-net.el ends here
