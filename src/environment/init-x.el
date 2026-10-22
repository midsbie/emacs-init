;;; init-x.el --- Graphical display configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(when window-system
  (setopt speedbar-use-images nil)        ; do not use icons in `speedbar-mode'

  ;; Uncomment if desired that the window should be maximised
  ; (toggle-frame-maximized)

  ;; Highlighter
  ;;
  ;; ?????? Disabled as it was seriously degrading performance
  ;;
  ;; 260621 Circumstances which led to disabling of highlighter are unclear.
  ;;        Tentatively reactivating since many package upgrades have taken
  ;;        place since.
  (global-hl-line-mode 1)

  ;; Enable clipboard functionality
  (setopt select-enable-clipboard t)

  ;; Load baseline theme and then overlay the personal theme on top.
  (load-theme 'my-tango-ansi t)
  (load-theme 'my-gui-dark t)
  (load-theme 'my-common t)

  ;; For some reason, version >25.x seems to activate the horizontal scrollbars.
  ;; We forcefully disable them here.
  (when (boundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  )

(provide 'init-x)

;;; init-x.el ends here
