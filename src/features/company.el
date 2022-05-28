;;; company.el --- Configures `company-mode' and related packages

;; Copyright (C) 2017-2022  Miguel Guedes

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

(defvar init/company/clear-flycheck-errors-timer nil)
(defvar init/company/clear-flycheck-errors-delay .5)

(defvar init/company/disabled-backends
  '(company-semantic company-bbdb company-gtags company-etags company-oddmuse
                     company-keywords))

(defun init/company/filter-backends (seq)
  "Remove company backends from SEQ.

Filter backends from `company-backends' that are specified in
`init/company/disabled-backends'.  Supports lists as elements in
`company-backends'."
  (let ((result))
    (dolist (elt seq)
      (let (interm)
        (setq interm (if (listp elt)
                    (init/company/filter-backends elt)
                  (unless (memq elt init/company/disabled-backends)
                    elt)))
        (when interm
          (setq result (cons interm result)))))
    (reverse result)))

(defun init/config/company ()
  "Configure `company'."
  (global-company-mode 1)

  ;; Add clang backend to disabled company backends unless the clang binary is
  ;; installed on the system.
  (unless clangd-p
    (add-to-list 'init/company/disabled-backends 'company-clang))

  ;; Remove backends specified in `init/company/disabled-backends'
  (setq company-backends (init/company/filter-backends company-backends))

  ;; The following disabled as it is believed to be involved in significant
  ;; performance degradation experienced in specific circumstances where the
  ;; number of candidates is particularly high.
  ;;
  ;; (company-statistics-mode)
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
  (when init/company/clear-flycheck-errors-timer
    (cancel-timer init/company/clear-flycheck-errors-timer))
  (setq init/company/clear-flycheck-errors-timer
        (run-with-idle-timer init/company/clear-flycheck-errors-delay
                             nil #'(lambda ()
                                     (flycheck-clear)))))

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
  ;; Note that some modes may need to redefine a higher minimum prefix length to
  ;; prevent weird interactions with LSP mode that may lead to degraded to
  ;; performance.
  (company-minimum-prefix-length 1)
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
