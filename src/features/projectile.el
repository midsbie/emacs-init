;;; projectile.el --- Initialises the projectile package

;; Copyright (C) 2017-2020  Miguel Guedes

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

(defun init/projectile ()
  "Initialise `projectile'."
  ;; For some strange reason this now needs to be set otherwise the projectile
  ;; keymap is not available at all.
  (setq-default projectile-keymap-prefix "")

  ;; Let projectile use ivy
  (setq projectile-completion-system 'ivy)

  (projectile-mode))

;; This statement was producing an error when placed before the function it
;; invokes, presumable because 'ido may have loaded in some circumstances.
(use-package projectile
  :diminish
  :after ido
  ;; Cannot be bound to `projectile-mode-map' or it won't work
  :bind (("C-c C-p p" . projectile-switch-project-action)
         ("C-c C-p d" . projectile-find-dir)
         ("C-c C-p f" . projectile-find-file))
  :init (init/projectile)
  :config (projectile-mode))

;;; projectile.el ends here
