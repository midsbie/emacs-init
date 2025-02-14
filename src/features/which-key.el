;;; which-key.el --- Configures the which-key package

;; Copyright (C) 2021-2025  Miguel Guedes

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

;; 220724 `which-key' package has been integrated into Emacs v30 and has been
;; removed from our internal ELPA repository.
;;
;; Initialization mostly from: https://github.com/justbur/emacs-which-key
;; ...which seems to have been taken from:
;; https://elpa.gnu.org/packages/which-key.html#orgf035424

;;; Code:

(defun init/which-key ()
  "Initialise the `which-key' package."
  ;; Make sure which-key doesn't show automatically, as is default behavior, and
  ;; refreshes quickly after it is triggered.
  (setq which-key-idle-delay            10000
        which-key-idle-secondary-delay  0.05
        ;; Allow C-? to trigger which-key manually
        which-key-show-early-on-C-h     t
        which-key-side-window-max-width 0.5))

(if (< emacs-major-version 30)
    (use-package which-key
      :diminish
      :init
      (init/which-key)
      :config
      (which-key-setup-side-window-right-bottom)
      (which-key-mode))
  (init/which-key)
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

;;; which-key.el ends here
