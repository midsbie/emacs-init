;;; mini-echo.el --- Configures the mini-echo unified mode line  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Replaces per-window mode-lines with a single unified status line displayed in
;; the echo area, showing information about the active buffer only.

;;; Code:

(use-package mini-echo
  :custom-face
  (mini-echo-blue ((t (:foreground "#729fcf"))))
  :config
  (setq mini-echo-persistent-rule
        '(:long  ("major-mode" "shrink-path" "vcs" "buffer-position"
                  "buffer-size" "flymake" "flycheck" "eglot")
          :short ("buffer-name" "buffer-position" "flymake" "flycheck")))
  (mini-echo-mode 1))

;;; mini-echo.el ends here
