;;; init-display-line-numbers.el --- Configure display-line-numbers  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/display-line-numbers/config ()
  "Lazily load the `display-line-numbers-mode' package and initialise it."

  ;; display line numbers in left margin
  (global-display-line-numbers-mode t))

(use-package display-line-numbers
  :ensure nil
  :custom
  ;; Markdown text sitting within `fill-column' was found wrapping early in
  ;; `visual-fill-column-mode'.  Line numbers are drawn inside the text area,
  ;; which that mode sizes to `fill-column', so the gutter took 4 to 5 columns
  ;; from the text.  Since version 2.2, visual-fill-column leaves this to its
  ;; users through `visual-fill-column-extra-text-width', which
  ;; `init/visual-fill-column/enable' sets to the gutter's width.  By default,
  ;; though, the gutter is sized for the lines in view and widens while
  ;; scrolling, which nothing would track.  This option sizes it once, for the
  ;; buffer's line count, when the mode is enabled.
  (display-line-numbers-width-start t)
  :config (init/display-line-numbers/config))

(provide 'init-display-line-numbers)

;;; init-display-line-numbers.el ends here
