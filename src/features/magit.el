;;; magit.el --- Configures the magit feature

;; Copyright (C) 2015-2018  Miguel Guedes

;; Author: Miguel Guedes <miguel@miguelguedes.org>
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

(require 'magit-status)

(with-eval-after-load 'magit-status
  ;; magit-status.el sets up C-x g but only after it is first run, however it
  ;; is desirable for the shortcut to be available immediately.
  (global-set-key (kbd "C-x g") 'magit-status)

  ;; NOTE: the following is disabled as it causes 'magit-status to not load.
  ;; Unclear why.
  ;; --
  ;; This undoes the advice found in magit-autoloads.el and allows our shortcut
  ;; to work (see `../internals/remaps.el`).
  ;(define-key ido-completion-map (kbd "C-x g") 'find-grep)
  ;(define-key ido-common-completion-map (kbd "C-x g") 'find-grep)
  )


;;; magit.el ends here
