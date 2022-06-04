;;; features.el --- simple configuration meta-module

;; Copyright (C) 2022  Miguel Guedes

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package buf-move
  :bind (("<C-S-up>" . buf-move-up)
         ("<C-S-down>" . buf-move-down)
         ("<C-S-left>" . buf-move-left)
         ("<C-S-right>" . buf-move-right)))

(use-package eww
  :bind (("C-c w g" . google)
         ("C-c w b" . browse-web)))

(use-package erc
  :init
  (setq erc-modules
        (quote (autojoin button completion fill irccontrols list match menu
                         move-to-prompt netsplit networks noncommands notifications
                         readonly ring stamp track))))

(use-package gcmh
  :diminish 'gcmh-mode
  :init
  (setq gcmh-idle-delay 5
	  gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
  :config (gcmh-mode))

(use-package neotree
  :init
     (setq neo-window-width 50
           neo-smart-open   t
           neo-theme        "nerd"))

(use-package recursion-indicator
  :demand t
  :init
  (recursion-indicator-mode t))

(use-package visual-regexp
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)))

;;; features.el ends here
