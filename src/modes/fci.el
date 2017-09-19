;;; fci.el --- Configures `fci-mode'

;; Copyright (C) 2015-2017  Miguel Guedes

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

(init/lazy-run 'init/fci)

(defun init/fci ()
  "Lazily load the `fill-column-indicator' package and configure it."
  (load "fill-column-indicator")
  (setq-default fci-rule-color "gray9"))

;; Workaround for fci-mode.  Don't enable fci-mode if emacs version is 24.3
;; since a bug was introduced affecting cursor navigation.
(defun enable-fci-mode ()
  "Enable `fci-mode'."
  (interactive)
  (if (and (eq emacs-major-version 24)
           (eq emacs-minor-version 3))
      (fci-mode -1)
    (fci-mode)))

;; Workaround for `company-mode'.  Disables `fci-mode' if it is currently
;; active to prevent visual corruption.
(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))

(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

;; Workaround for auto-complete.  Disable fci-mode if pop up created or it will
;; lead to visual corruption; re-enable when all popups closed.
(defvar emacsinit/fci-mode-suppressed nil)
(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible."
  (when (not emacsinit/fci-mode-suppressed)
    (set (make-local-variable 'emacsinit/fci-mode-suppressed) fci-mode)
    (when fci-mode
      (turn-off-fci-mode))))

(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed."
  (when (and (not popup-instances) emacsinit/fci-mode-suppressed)
    (setq emacsinit/fci-mode-suppressed nil)
    (turn-on-fci-mode)))

;;; fci.el ends here
