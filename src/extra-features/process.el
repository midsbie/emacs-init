;;; process.el --- Convenience functions for process management

;; Copyright (C) 2023  Miguel Guedes

;; Author: Miguel Guedes <miguel@softgeist.com>
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

(defun my/run-or-load (func library)
  "Execute FUNC or load LIBRARY if FUNC not defined.
Checks that the defun FUNC has been loaded before invoking it.  If it
hasn't been loaded, LIBRARY is loaded via a call to `load-library'."
  (message "checking: %s" (or (and (fboundp func) "YES") "NO") )
  (unless (fboundp func)
    (load-library (symbol-name library)))
  (call-interactively func))

(defmacro my/t-run-or-load (func library)
  "Return a lambda that can be attached to a key handler or similar handlers.
See documentation of `run-or-load' for a description of the FUNC
and LIBRARY variables."
  #'(lambda()
      (interactive)
      (run-or-load ,func ,library)))

(defun my/kill-current-buffer ()
  "Simply deletes the currently active buffer.  Overcomes the
issues that `kill-this-buffer' is sometimes afflicted by where it
stops working.  This [1] post over on Reddit explains what
happens.

[1] https://www.reddit.com/r/emacs/comments/64xb3q/killthisbuffer_sometimes_just_stops_working/"
  (interactive)
  (kill-buffer (current-buffer)))

;;; process.el ends here