;;; init-copilot.el --- Configure copilot  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Documentation found at the repository:
;; https://github.com/lanceberge/copilot

;;; Code:

(defun init/copilot/complete ()
  "Complete at the current point."
  (interactive)
  (corfu-quit)
  (copilot-complete))

(defun init/copilot/consume-typed-completion (command)
  "Dismiss a one-character completion that COMMAND has just typed.
When the typed character is the whole completion, `copilot--self-insert'
accepts it, deleting and re-inserting text between positions recorded
before the command ran.  `electric-indent-mode' re-indents the line in
between (e.g. moving `}' to its block's column), so the stale positions
either double the character and delete text after point, or undo the
re-indentation.  The character is already in the buffer, so only the
overlay needs to go."
  (when (and (eq command 'self-insert-command)
             (characterp last-command-event)
             (equal (copilot-current-completion) (string last-command-event))
             (copilot--satisfy-display-predicates))
    (copilot-clear-overlay t)
    t))

(defvar init/copilot/server-warned nil
  "Non-nil once the missing language server has been reported.")

(defun init/copilot/enable ()
  "Enable `copilot-mode' when its language server is installed.
Otherwise warn, once per session.  Install the server with
\\[copilot-install-server]."
  (when (require 'copilot nil t)
    (cond
     ((ignore-errors (copilot-server-executable))
      (copilot-mode 1))
     ((not init/copilot/server-warned)
      (setq init/copilot/server-warned t)
      (display-warning 'copilot "Language server not installed; `copilot-mode' \
disabled.  Install it with M-x copilot-install-server.")))))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :diminish
  :hook (prog-mode . init/copilot/enable)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))
  (:map copilot-mode-map
        ("C-<tab>" . 'init/copilot/complete))
  :custom
  (copilot-idle-delay 0)
  :config
  (advice-add 'copilot--self-insert :before-until #'init/copilot/consume-typed-completion))

(provide 'init-copilot)

;;; init-copilot.el ends here
