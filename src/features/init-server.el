;;; init-server.el --- Configure server  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun init/server/config ()
  "Start Emacs server.

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
  :ensure nil
  :config (init/server/config))

(provide 'init-server)

;;; init-server.el ends here
