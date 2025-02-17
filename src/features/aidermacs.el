;;; aidermacs.el --- Customises the aidermacs package

;; Copyright (C) 2025  Miguel Guedes

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
;; https://github.com/MatthewZMD/aidermacs

;;; Code:

(defun init/setup-aidermacs ()
  "Set up the aidermacs package.
This function checks if 'aider' is available, unsets the keybinding for
C-c a, configures the aidermacs package, and sets the keybinding for C-c
a to initialize aidermacs on first use."
  (interactive)

  ;; Check if 'aider' is available
  (unless (executable-find "aider")
    (user-error "Aider is not found. Please install it."))

  (global-unset-key (kbd "C-c a"))

  (use-package aidermacs
    :vc (:url "git@github.com:MatthewZMD/aidermacs.git"
              :rev :newest
              :branch "main")
    :config
    ;; Notes regarding models:
    ;;
    ;; - "o3-mini" is available only for higher usage tiers for the time being
    ;; - "o3-mini-high" may be available
    ;; - "codestral/codestral-latest" seems to be free for the moment.
    (setq aidermacs-args '("--model" "gpt-4o-mini"))
    (setenv "OPENAI_API_KEY" (my/read-authsource-secret "api.openai.com" "apikey"))
    (setenv "CODESTRAL_API_KEY" (my/read-authsource-secret "codestral.mistral.ai" "apikey"))
    (global-set-key (kbd "C-c a") 'aidermacs-transient-menu))
  (aidermacs-transient-menu))

;; Bind C-c a to initialize aidermacs on first use.
(global-set-key (kbd "C-c a") #'init/setup-aidermacs)

;;; aidermacs.el ends here
