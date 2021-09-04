;;; typescript.el --- Configures `typescript-mode'

;; Copyright (C) 2020-2021  Miguel Guedes

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

;;; Log:
;; 21-09-03 Disabling tide in favour of LSP

;;; Code:

(defun init/mode/ts-tsx ()
  "Initialise modes related to Typescript development."
  (init/common-web-programming)
  (setq-local typescript-indent-level 2)

  (local-set-key (kbd "M-a") 'c-beginning-of-statement)
  (local-set-key (kbd "M-e") 'c-end-of-statement)

  (init/mode/ts-tsx/lsp-or-tide))

(defun init/mode/tsx ()
  "Initialise typescript mode for JSX."
  ; Since web-mode may be started for a wide variety of source files, such as
  ; HTML markup, template files and Javascript, the initialisation is only run
  ; if the buffer's file extension suggests a typescript source file.
  (when (string-equal "tsx" (file-name-extension buffer-file-name))
    (init/mode/ts-tsx)))

(defun init/mode/ts-tsx/lsp-or-tide ()
  "Initialise LSP or TIDE for typescript."
  (init/mode/ts-tsx/lsp)
  ;; (init/mode/tide)
  )

(defun init/mode/ts-tsx/lsp ()
  "Initialise LSP for typescript."
  ; For some reason, if LSP is invoked preemptively in this mode hook, it will
  ; initialise correctly but will fail to work as expected: lsp-ui will not
  ; work, nor will definition at point, and other features.  Solution found was
  ; to lazy-initialise it.
  ;;  (lsp))
  (run-with-idle-timer .1 nil '(lambda()
                                 (lsp)
                                 ;; Unfortuntaly LSP's code lenses aren't yet supported by the
                                 ;; server.  Here for posteriority, however.
                                 ;; (lsp-lens-show)
                                 )))

(defun init/mode/tide()
  "Initialise tide mode."
  (interactive)
  (message "TIDE HAS BEEN DEPRECATED: USE LSP INSTEAD")
  (tide-setup)

  (setq-local flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq-local company-tooltip-align-annotations t)

  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint)
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)

  (eldoc-mode 1)
  (tide-hl-identifier-mode 1))

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . web-mode))
  :hook
  (typescript-mode . init/mode/ts-tsx)
  (web-mode-hook . init/mode/tsx))

;; (use-package tide
;;   :ensure t
;;   :after (typescript-mode company flycheck)
;;   :hook (typescript-mode . init/mode/tide)
;;     :bind ((:map tide-mode-map
;;                  ("C-c t s" . tide-restart-server)
;;                  ("C-c t S" . tide-kill-server)
;;                  ("C-c t p" . tide-project-errors)
;;                  ("C-c C-c" . tide-fix)
;;                  ("C-c t x" . tide-references)
;;                  ("C-c t m" . tide-rename-symbol)
;;                  ("C-c t M" . tide-rename-file)
;;                  ("C-c t r" . tide-refactor)))
;;   :init
;;   (setq tide-server-max-response-length 1024000) ; x10 the default value
;;   )

;;; typescript.el ends here
