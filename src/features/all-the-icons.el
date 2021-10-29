;;; all-the-icons.el --- Configures all-the-icons and related packages

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
;; This module configures the all-the-icons package but also all packages
;; related to it.

;;

;;; Code:

(defun init/all-the-icons-completion ()
  ;; This package doesn't seem to do anything but here for now while it is
  ;; evaluated.
  ;; Ref: https://github.com/iyefrat/all-the-icons-completion
  (all-the-icons-completion-mode))

(use-package all-the-icons-completion
  :init
  (init/all-the-icons-completion))

;; Ref: https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons
  ;; Ref: https://github.com/wyuenho/all-the-icons-dired
  :hook ((dired-mode . all-the-icons-dired-mode)))

;; Ref: https://github.com/seagle0128/all-the-icons-ibuffer
(use-package all-the-icons-ibuffer
  :init (all-the-icons-ibuffer-mode 1))

;; Ref: https://github.com/seagle0128/all-the-icons-ivy-rich
(use-package all-the-icons-ivy-rich
  ;; Only loads after the ivy-rich package does
  :after (ivy-rich)
  :init (all-the-icons-ivy-rich-mode 1))

;;; all-the-icons.el ends here
