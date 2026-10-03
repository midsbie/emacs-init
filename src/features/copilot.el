;;; copilot.el --- Customises the copilot package

;; Copyright (C) 2024-2026  Miguel Guedes

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

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :diminish
  :hook (prog-mode . copilot-mode)
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

;;; copilot.el ends here