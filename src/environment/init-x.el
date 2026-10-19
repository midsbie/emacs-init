;;; init-x.el --- Initialisation sequence when run within X  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; URL:
;; Keywords: tools
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

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


  ;; Default face font (theme handles colors).  Keep it in step with the
  ;; `font' frame parameter in early-init.el.
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil
                           :inverse-video nil :box nil
                           :strike-through nil :overline nil :underline nil
                           :slant normal :weight normal :height 85 :width normal
                           :family "Hack")))))

  ;; Load baseline theme and then overlay user-selected theme on top.
  (load-theme 'my-tango-ansi t)
  (when init/theme
    (load-theme init/theme t))

  ;; For some reason, version >25.x seems to activate the horizontal scrollbars.
  ;; We forcefully disable them here.
  (when (boundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  )

(provide 'init-x)

;;; init-x.el ends here
