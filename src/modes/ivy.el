;;; ivy.el --- Configures `ivy-mode'

;; Copyright (C) 2019-2020  Miguel Guedes

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

(defun init/config/ivy()
  (use-package flx
    :ensure t)
  (use-package swiper
    :ensure t)
  (use-package counsel
    :ensure t)

  ;; Activate ivy's global mode
  ;; Note that we choose not to lazy load this package since it is seen as being
  ;; of primary importance to the Emacs user experience and should therefore be
  ;; immediately available as soon as the editor finishes loading is accepting
  ;; user input.
  (ivy-mode 1)

  ;; From: https://www.reddit.com/r/emacs/comments/51lqn9/helm_or_ivy/d7d34il/
  ;;       https://emacs.stackexchange.com/a/36748
  ;; Let ivy use flx for fuzzy-matching for everything but swiper!
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t      . ivy--regex-fuzzy)))

  ;; Use Enter on a directory to navigate into the directory, not open it with dired.
  (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)

  ;; Let projectile use ivy
  (setq projectile-completion-system 'ivy)

  ;; Suggestions from: https://oremacs.com/swiper/#basic-customization
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")

  ;; The following requires the package "swiper" to be installed
  (global-set-key (kbd "C-s") 'swiper)

  ;; Counsel-based key mappings
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-x h a") 'counsel-apropos)
  (global-set-key (kbd "<f1> a") 'counsel-apropos)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "C-x h f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "C-x h v") 'counsel-describe-variable)
  ;; Remap current C-x h l mapping to C-x h L
  (global-set-key (kbd "C-x h L") 'view-lossage)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "C-x h l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)

  ;; Ivy-based interface to shell and system tools
  (global-set-key (kbd "C-c c") 'counsel-compile)
  ;; (global-set-key (kbd "C-c G") 'counsel-git)
  ;; The following is disabled as it was causing emacs to hang and leak memory.
  ;; We are now using the custom `git-grep` interactive function instead.
  ;; (global-set-key (kbd "C-c g") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)

  ;; Resume the last ivy-based completion
  (global-set-key (kbd "C-c C-r") 'ivy-resume)

  ;; Set height of completion mini-buffer
  (setq ivy-height 20))

(use-package ivy
  :config
  (init/config/ivy))

;;; ivi.el ends here
