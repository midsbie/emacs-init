;;; server.el --- Configuration of Emacs `server' package

;; Copyright (C) 2017-2024  Miguel Guedes

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

(defun init/server/config ()
  "Start Emacs server.

Also visits the files in the `init/open-at-startup' list if the
server hasn't yet been started.

Strangely, the call to `server-start' needs to be issued a few
seconds after Emacs has launched."

  (if (server-running-p)
      (message "[server] already started: not starting")

    ;; Start server after a minor delay.
    (run-with-idle-timer 1 nil
                         #'(lambda ()
                             (message "[server] starting")
                             (server-start)))))

(use-package server
  :config (init/server/config))

;;; server.el ends here
