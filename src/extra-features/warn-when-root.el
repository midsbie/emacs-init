;;; warn-when-root.el --- Change modeline background when acting as root user

;; Copyright (C) 2015-2023  Miguel Guedes

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


(defun my/warn-when-root-visits-file ()
  "Warn when root user.

Displays the mode line with a red background when visiting a file
with root privileges."
  (when (string-match "^/su\\(do\\)?:" default-directory)
    (face-remap-add-relative
     'mode-line
     '(:background "red3" :foreground "white"))
    (face-remap-add-relative
     'mode-line-inactive
     '(:background "red4" :foreground "dark gray"
                   :box nil))
    )
  )

;; Change mode line background colour if file being edited as root.
(add-hook 'after-change-major-mode-hook 'my/warn-when-root-visits-file)

;;; warn-when-root.el ends here
