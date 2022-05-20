;;; company.el --- Configures `company-mode' and related packages

;; Copyright (C) 2017-2021  Miguel Guedes

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

(defun init/config/company ()
  "Configure `company'."
  (unless clangd-p (delete 'company-clang company-backends))
  (global-company-mode 1)
  (company-statistics-mode)
  (add-hook 'company-completion-started-hook 'my/company/clear-flycheck-errors)
  (define-key company-mode-map (kbd "<C-return>") 'company-complete))

(defun init/config/company-box ()
  ;; company-box sets the `company-tooltip-selection' face invisible
  (custom-set-faces
   '(company-tooltip-selection
     ((((class color) (min-colors 88) (background light))
       (:background "light blue"))
      (((class color) (min-colors 88) (background dark))
       (:background "gray31"))
      (t (:background "green"))))
   '(company-tooltip ((t (:background "grey3"))))
   ))

(defun my/company/clear-flycheck-errors (manual)
  "Clear flycheck status in buffer."
  (flycheck-clear))

(defun smarter-yas-expand-next-field-complete ()
  "Try to `yas-expand' and `yas-next-field' at current cursor position.

If failed try to complete the common part with `company-complete-common'"
  (interactive)
  (if yas-minor-mode
      (let ((old-point (point))
            (old-tick (buffer-chars-modified-tick)))
        (yas-expand)
        (when (and (eq old-point (point))
                   (eq old-tick (buffer-chars-modified-tick)))
          (ignore-errors (yas-next-field))
          (when (and (eq old-point (point))
                     (eq old-tick (buffer-chars-modified-tick)))
            (company-complete-common))))
    (company-complete-common)))

;; As per: https://github.com/MatthewZMD/.emacs.d#company-mode
(use-package company
  :diminish
  :hook ((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
  :bind
  (:map company-active-map
        ([tab] . smarter-yas-expand-next-field-complete)
        ("TAB" . smarter-yas-expand-next-field-complete))
  :custom
  ;; This was previously set to 1 but it was found to cause micro-stutters in
  ;; some circumstances where the list of candidates returned by by the LSP
  ;; server was too large.  Notably it was happening fairly consistently when
  ;; editing specific Dart/Flutter source files.  Since the increase to 2,
  ;; editing is no longer the pain it sometimes was.
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  (company-begin-commands '(self-insert-command))
  (company-require-match 'never)
  ;; Don't use company in the following modes
  (company-global-modes '(not shell-mode eaf-mode))
  ;; DON'T set this to 0 to trigger immediate completion as it causes LSP to
  ;; spam the server it is connected to leading to degradation of performance
  ;; and, in some cases (e.g. Unity C# source files), rendering the editor
  ;; unusable.
  (company-idle-delay 0.2)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)
  (company-tooltip-idle-delay 0.1)
  :config
  (init/config/company))

(use-package company-lsp
  :defer t
  :custom (company-lsp-cache-candidates 'auto))

;; Ref: https://github.com/sebastiencs/company-box
(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode)
  :config (init/config/company-box))

;;; company.el ends here
