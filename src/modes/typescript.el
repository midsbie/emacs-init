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

(defun init/ts-tsx-mode ()
  "Initialise modes related to Typescript development."
  (init/common-web-programming)
  ;; Disable flow-minor-mode regardless since there's a chance web-mode may initialise ahead of
  ;; tide-mode
  (flow-minor-mode -1)

  (setq-local fill-column 99)
  (setq-local typescript-indent-level 2)

  (local-set-key (kbd "M-a") 'c-beginning-of-statement)
  (local-set-key (kbd "M-e") 'c-end-of-statement))

(defun init/tide()
  "Initialise tide mode."
  (interactive)
  (tide-setup)

  (setq-local flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq-local company-tooltip-align-annotations t)

  (eldoc-mode 1)
  (tide-hl-identifier-mode 1))

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . web-mode))
  :init
  (add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (init/ts-tsx-mode)
              (init/tide))))
  :hook (typescript-mode . init/ts-tsx-mode))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook (typescript-mode . init/tide)
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
