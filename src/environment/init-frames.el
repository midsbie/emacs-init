;;; init-frames.el --- Display environment selection  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Load init-x or init-term when its display type first appears, including
;; graphical clients created after daemon startup.

;;; Code:

(defcustom init/theme 'my-gui-dark
  "Theme to load during GUI initialization.
Can be `my-gui-dark', `my-gui-light', or any other installed theme.
Terminal Emacs uses `wombat' with `my-term-dark' layered on top."
  :group 'init
  :type 'symbol)

(defun init/configure-frame (frame)
  "Load the environment configuration once for FRAME's display type."
  (with-selected-frame frame
    (require (if (display-graphic-p frame) 'init-x 'init-term))))

(add-hook 'after-make-frame-functions #'init/configure-frame)
(init/configure-frame (selected-frame))

(provide 'init-frames)

;;; init-frames.el ends here
