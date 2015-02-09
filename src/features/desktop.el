;;; desktop.el --- Configures the desktop feature

;; Copyright (C) 2015  Miguel Guedes

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

;; Set our preferred window arrangement after successfully loading a desktop
;; configuration.
(add-hook 'desktop-after-read-hook
          '(lambda ()
             (let ((windows (- (/ (frame-width) 80) 1)))
               (dotimes (n windows)
                 (split-window-right))
               (balance-windows)
               (dotimes (n windows)
                 (other-window 1)
                 (bury-buffer))
               (other-window 1))))

;;; desktop.el ends here