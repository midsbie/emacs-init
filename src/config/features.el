;;; features.el --- master configuration module of features

;; Copyright (C) 2022-2026  Miguel Guedes

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

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :config
  ;; Revert buffer if file associated with it changes outside of emacs.
  (global-auto-revert-mode 1))

(use-package browse-kill-ring
  :defer t)

(use-package buffer-move
  :bind (("<C-S-up>" . buf-move-up)
         ("<C-S-down>" . buf-move-down)
         ("<C-S-left>" . buf-move-left)
         ("<C-S-right>" . buf-move-right)))

(use-package dabbrev
  :ensure nil
  :custom
  ;; Make it so matching and expansion respects case.  May need to be set for
  ;; prog modes only.
  (dabbrev-case-fold-search nil)
  (dabbrev-case-replace nil))

(use-package erc
  :ensure nil
  :init
  (setq erc-modules
        (quote (autojoin button completion fill irccontrols list
                         match menu move-to-prompt netsplit
                         networks noncommands notifications
                         readonly ring stamp track))))

(use-package display-fill-column-indicator
  :ensure nil
  :diminish
  ;; Opt-in rather than global: avoids the indicator in buffers where
  ;; fill-column is meaningless (terminals, special buffers).  conf-mode and
  ;; yaml-ts-mode don't derive from prog-mode, and org-mode derives from
  ;; text-mode but doesn't benefit from the indicator, so they need explicit
  ;; hooks/overrides.
  :hook ((prog-mode text-mode conf-mode yaml-ts-mode)
         . display-fill-column-indicator-mode)
  :config
  (add-hook 'org-mode-hook
            (lambda () (display-fill-column-indicator-mode -1))))

(use-package highlight-parentheses
  :diminish
  :config (global-highlight-parentheses-mode))

(use-package flyspell
  :ensure nil
  :diminish
  :config
  ;; Deactivate annoying correction of previous misspelled error when C-; is hit.
  ;; NOTE: currently enabled
  ;; (define-key flyspell-mode-map (kbd "C-;") nil)
  )

(use-package gcmh
  :diminish
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
  :diminish
  :config
  (recursion-indicator-mode t))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :config
  (savehist-mode))

(use-package simple
  :ensure nil
  :diminish auto-fill-mode)

(use-package subword
  :ensure nil
  :diminish)

;;; features.el ends here
