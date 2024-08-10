;;; ansi-color.el --- Customises ANSI color palette

;; Copyright (C) 2015-2024  Miguel Guedes

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

(defun init/ansi-color/config ()
  "Initialize the `ansi-color' package."
  (setq ansi-color-names-vector ["black" "red" "lawn green" "yellow"
                                 "cornflower blue" "magenta" "cyan" "white"]
        ansi-color-map          (ansi-color-make-color-map)))

(defun my/colorize-buffer ()
  "Colorize the entirety of the current buffer if not read-only."
  (interactive)
  (if buffer-read-only
      (message "Unable to colorize read-only buffer")
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun my/colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(use-package ansi-color
  :hook ((compilation-filter . my/colorize-compilation-buffer))
  :config (init/ansi-color/config))

;;; ansi-color.el ends here
