;;; init-server.el --- Configure server  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defvar init/server/start-timer nil
  "Idle timer scheduled to start the Emacs server, or nil.")

(defun init/server/start ()
  "Start the Emacs server unless one with its name is already running."
  (setq init/server/start-timer nil)
  (if (server-running-p)
      (message "[server] already started: not starting")
    (message "[server] starting")
    (server-start)))

(defun init/server/config ()
  "Start the Emacs server once Emacs is idle.

Strangely, the call to `server-start' needs to be issued a few
seconds after Emacs has launched.  A daemon starts its own server
after init, so it is left alone."
  (unless (daemonp)
    (if (server-running-p)
        (message "[server] already started: not starting")
      (unless init/server/start-timer
        (setq init/server/start-timer
              (run-with-idle-timer 1 nil #'init/server/start))))))

(use-package server
  :ensure nil
  :config (init/server/config))

(provide 'init-server)

;;; init-server.el ends here
