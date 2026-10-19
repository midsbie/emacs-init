;;; init-buffer-move.el --- Configure buffer-move -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configuration for the buffer-move library.

;;; Code:

(use-package buffer-move
  :bind (("<C-S-up>" . buf-move-up)
         ("<C-S-down>" . buf-move-down)
         ("<C-S-left>" . buf-move-left)
         ("<C-S-right>" . buf-move-right)))

(provide 'init-buffer-move)

;;; init-buffer-move.el ends here
