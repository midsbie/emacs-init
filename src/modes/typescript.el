;;; typescript.el --- Configures `typescript-mode'

;; Copyright (C) 2020  Miguel Guedes

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

(defun init/mode/tsx ()
  "Initialise typescript mode for JSX."
  (when (string-equal "tsx" (file-name-extension buffer-file-name))
    (init/mode/ts-tsx)
    (init/mode/tide)))

(defun init/mode/ts-tsx ()
  "Initialise modes related to Typescript development."
  (init/common-web-programming)
  (setq-local typescript-indent-level 2)
  
  (local-set-key (kbd "M-a") 'c-beginning-of-statement)
  (local-set-key (kbd "M-e") 'c-end-of-statement))

(defun init/mode/tide()
  "Initialise tide mode."
  (interactive)
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
  :init
  (add-hook 'web-mode-hook 'init/mode/tsx)
  :hook (typescript-mode . init/mode/ts-tsx))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook (typescript-mode . init/mode/tide)
  :bind ((:map tide-mode-map
               ("C-c t s" . tide-restart-server)
               ("C-c t S" . tide-kill-server)
               ("C-c t p" . tide-project-errors)
               ("C-c C-c" . tide-fix)
               ("C-c t x" . tide-references)
               ("C-c t m" . tide-rename-symbol)
               ("C-c t M" . tide-rename-file)
               ("C-c t r" . tide-refactor)))
  :init
  (setq tide-server-max-response-length 1024000) ; x10 the default value
  )

;;; typescript.el ends here
