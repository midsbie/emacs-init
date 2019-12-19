;;; term.el --- Initialisation sequence when run from the terminal

;; Copyright (C) 2014-2018 Miguel Guedes

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

(eval-after-load 'linum
  '(progn
     ; Load an appropriate theme for a terminal and set the background color to
     ; as close to black as we can.
     (load-theme 'wombat)
     (set-background-color "color232")

     ;; Customize faces that turn out not to support a terminal environment.
     (custom-set-faces
      ;; custom-set-faces was added by Custom.
      ;; If you edit it by hand, you could mess it up, so be careful.
      ;; Your init file should contain only one such instance.
      ;; If there is more than one, they won't work right.
      '(magit-section-highlight ((t (:background "color-235"))))
      '(hl-line ((t (:background "color-235" :underline nil)))))

     ; Turn highlight one globally (configuration is above)
     ; UPDATE: disabled as it was degrading performance
     ;; (global-hl-line-mode 1)

     ; Fix "stuck" fringe by adding a space between line number and actual
     ; content
     (unless (eq window-system 'x)
       (setq linum-format "%d "))))


;;; term.el ends here
