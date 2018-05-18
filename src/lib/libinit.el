;;; libinit --- Collection of useful defuns for use at initialisation time

;; Copyright (C) 2017-2018 Miguel Guedes

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
;; Used by init.el.

;;; Code:

(defun init/lazy-run (function &rest args)
  "Lazily run FUNCTION after a set amount of time elapses.

The function is only executed when the editor is in an idle state."
  (run-with-idle-timer 1 nil function))

(provide 'libinit)

;;; libinit.el ends here
