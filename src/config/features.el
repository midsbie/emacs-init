;;; features.el --- master configuration module of features

;; Copyright (C) 2022-2025  Miguel Guedes

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

;; This module contains use-package invocations and configurations for features
;; for which a dedicated configuration module was thought to be unjustified.

;;; Code:

(use-package ace-window
  :bind ("M-o" . ace-window))

;; This package is required by `init/prettier-mode/maybe-enable' and all
;; web-related source files..
(use-package add-node-modules-path)

(use-package autorevert
  :diminish
  :config
  ;; Revert buffer if file associated with it changes outside of emacs.
  (global-auto-revert-mode 1))

(use-package buf-move
  :bind (("<C-S-up>" . buf-move-up)
         ("<C-S-down>" . buf-move-down)
         ("<C-S-left>" . buf-move-left)
         ("<C-S-right>" . buf-move-right)))

(use-package dabbrev
  :custom
  ;; Make it so matching and expansion respects case.  May need to be set for
  ;; prog modes only.
  (dabbrev-case-fold-search nil)
  (dabbrev-case-replace nil))

(use-package erc
  :init
  (setq erc-modules
        (quote (autojoin button completion fill irccontrols list
                         match menu move-to-prompt netsplit
                         networks noncommands notifications
                         readonly ring stamp track))))

(use-package display-fill-column-indicator
  :diminish
  :config
  ;; This feature, now native to Emacs 27, replaces the old package
  ;; `fill-column-indicator' that was previously in use.
  (global-display-fill-column-indicator-mode))

(use-package highlight-parentheses
  :diminish
  :config (global-highlight-parentheses-mode))

(use-package flyspell
  :config
  ;; Deactivate annoying correction of previous misspelled error when C-; is hit.
  ;; NOTE: currently enabled
  ;; (define-key flyspell-mode-map (kbd "C-;") nil)
  )

(use-package gcmh
  :diminish 'gcmh-mode
  :init
  (setq gcmh-idle-delay 5
	      gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb

  :config
  (gcmh-mode))

(use-package marginalia
  :demand
  :config
  (marginalia-mode))

(use-package neotree
  :init
     (setq neo-window-width 50
           neo-smart-open   t
           neo-theme        "nerd"))

(use-package recursion-indicator
  :demand t
  :config
  (recursion-indicator-mode t))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :config
  (savehist-mode))

(use-package simple
  :diminish auto-fill-mode)

(use-package subword
  :diminish)

(use-package swiper
  :demand
  :bind ("C-s" . swiper-isearch))

;;; features.el ends here
