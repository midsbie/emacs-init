;;; init-term.el --- Terminal display configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Loaded once by `init/configure-frame' for the first terminal frame.

;;; Code:

;; Load wombat as the base, then layer our terminal theme on top.
(load-theme 'wombat t)
(load-theme 'my-term-dark t)

(provide 'init-term)

;;; init-term.el ends here
