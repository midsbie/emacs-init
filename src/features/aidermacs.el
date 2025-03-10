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
    :ensure t
    :vc (:url "git@github.com:MatthewZMD/aidermacs.git"
              :rev :newest
              :branch "main")

    :custom
    (aidermacs-default-model "deepseek/deepseek-chat")
    (aidermacs-architect-model "deepseek/deepseek-reasoner")
    (aidermacs-popular-models
     '("anthropic/claude-3-5-sonnet-20241022" ;; really good
       "openai/o3-mini"                       ;; very powerful
       "openai/o3-mini-high"                  ;; great for coding tasks
       "openai/gpt-4o-mini"
       "openai/gpt-4o"
       "gemini/gemini-2.0-flash"              ;; free
       "gemini/gemini-1.5-pro"                ;;
       "mistral/codestral-latest"
       ;; chatgpt-4o level performance, price is 1/100. weakness: small context
       "deepseek/deepseek-chat"
       "deepseek/deepseek-reasoner"
       ))

    :config
    (setenv "ANTHROPIC_API_KEY" (my/read-authsource-secret "api.anthropic.com" "apikey"))
    (setenv "DEEPSEEK_API_KEY" (my/read-authsource-secret "api.deepseek.com" "apikey"))
    (setenv "GEMINI_API_KEY" (my/read-authsource-secret "generativelanguage.googleapis.com" "apikey"))
    (setenv "MISTRAL_API_KEY" (my/read-authsource-secret "codestral.mistral.ai" "apikey"))
    (setenv "OPENAI_API_KEY" (my/read-authsource-secret "api.openai.com" "apikey"))
    (global-set-key (kbd "C-c a") 'aidermacs-transient-menu))
  (aidermacs-transient-menu))

;; Bind C-c a to initialize aidermacs on first use.
(global-set-key (kbd "C-c a") #'init/setup-aidermacs)

;;; aidermacs.el ends here
