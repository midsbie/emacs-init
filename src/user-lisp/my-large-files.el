;;; my-large-files.el --- Large file editing support  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

;;;###autoload
(defun my/handle-large-file-hook (&rest _args)
  "Use read-only Fundamental mode for files over 1,048,576 characters.
Return non-nil when handled, for use as before-until advice on
`set-auto-mode'.  Ignore that function's arguments."
  (when (and buffer-file-name (> (buffer-size) (* 1024 1024)))
    (fundamental-mode)
    (setq buffer-read-only t)
    (buffer-disable-undo)
    t))

(provide 'my-large-files)

;;; my-large-files.el ends here
