;;; init-programming.el --- Initialisation of programming related features

;; Copyright (C) 2014 Miguel Guedes
;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; URL:
;; Keywords: init
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; ----- Includes
;; - internal
(require 'cl-lib)

;; - from packages:
;; (load-library "doxymacs/doxymacs")

;; - using `load-path'
(load-library "buftoggle.git/buftoggle")
(load-library "builder.git/src/builder")

;; - from ELPA:
(require 'yasnippet)
(require 'php-mode)
(require 'highlight-parentheses)
(require 'js2-mode)
(require 'flycheck)
(require 'auto-complete-config)
(require 'web-mode)

;; auto-complete mode
(ac-config-default)
(global-auto-complete-mode)

(define-key ac-menu-map (kbd "C-n") 'ac-next)
(define-key ac-menu-map (kbd "C-p") 'ac-previous)

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default js2-basic-offset  2)

;; html-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; omnis
(when (file-exists-p "/usr/local/share/omnis/clients/omniscient/omniscient.el")
  (add-to-list 'load-path "/usr/local/share/omnis/clients/")
  (load-library "omniscient/omniscient.el")
  (load-library "omniscient/cc-mode.el"))

;; ----- Various settings
;; Various settings
(setq change-log-default-name "CHANGELOG")

;; create a fake awk-mode based on cc-mode
(autoload 'awk-mode "cc-mode" nil t)

;; yasnippet
(setq yas/snippet-dirs '("~/.emacs.d/snippets"))
(yas/global-mode 1)

;; buftoggle
(add-to-list 'buftoggle-pairs-alist '("cxx" "hxx" "hpp" "hh" "h"))
(add-to-list 'buftoggle-pairs-alist '("hxx" "cxx" "cpp" "cc"))

;; scss-mode
(setq scss-sass-command "scss")

;; Default coding style.
(c-add-style "default"
            '("linux"
              (c-recognize-knr-p . nil)
              (c-basic-offset  . 2)
              (indent-tabs-mode . nil)
              (c-comment-only-line-offset . 0)
              (c-syntactic-indentation-in-macros . nil)
              (c-hanging-braces-alist . ((brace-list-open)
                                         (brace-entry-open)
                                         (substatement-open after)
                                         (block-close . c-snug-do-while)
                                         (arglist-cont-nonempty)))
              (c-cleanup-list . (brace-else-brace
                                 brace-elseif-brace
                                 brace-catch-brace
                                 empty-defun-braces
                                 one-liner-defun
                                 defun-close-semi
                                 space-before-funcall
                                 compact-empty-funcall))

              (c-offsets-alist . ((innamespace           . 0)
                                  (statement-block-intro . +)
                                  (knr-argdecl-intro     . 0)
                                  (substatement-open     . 0)
                                  (substatement-label    . 0)
                                  (label                 . 0)
                                  (statement-cont        . +)))))

;; OpenBSD style.
(c-add-style "openbsd"
             '("bsd"
               (c-backspace-function . delete-backward-char)
               (c-syntactic-indentation-in-macros . nil)
               (c-tab-always-indent . nil)
               (c-hanging-braces-alist (block-close . c-snug-do-while))
               (c-offsets-alist (arglist-cont-nonempty . *)
                                (statement-cont . *))
               (indent-tabs-mode . t)))

;; default settings
(setq-default
 c-default-style    "default"           ; own default style
 tab-width          2
 indent-tabs-mode   nil
 ;; javascript
 js-indent-level    2
 ;; sh
 sh-basic-offset    2
 sh-indentation     2
 ;; php
 php-mode-coding-style      'drupal
 php-lineup-cascaded-calls  t)

;; flycheck
(global-flycheck-mode 1)

;; + in c/c++ modes
(setq flycheck-cppcheck-checks '("all")
      flycheck-c/c++-clang-executable "true") ; disable clang since we use omnis

;; + in php mode
(setq flycheck-php-phpmd-executable "phpmd"
      flycheck-phpmd-rulesets '("cleancode" "codesize" "controversial"
                                "design" "naming" "unusedcode"))

;; Set default product when on `sql-mode'
(setq sql-product 'mysql)

;; -----  Setup customisation of major modes.
(defun initialise-common-programming ()
  (enable-fci-mode)                 ; fill column indicator
  (auto-fill-mode)                  ; auto fill
  (flyspell-prog-mode)              ; turn spell check for strings and comments
  (ac-flyspell-workaround)          ; this defun must be executed to prevent
                                    ; flyspell from messing with
                                    ; auto-complete-mode
  (highlight-parentheses-mode)      ; turn on { } and ( ) highlighting
  (abbrev-mode -1)                  ; turn abbrev-mode off
  (subword-mode 1)

  ;; `c-auto-newline' was previously disabled and done so for a reason. Have
  ;; just tested it and seemed fine whilst editing a C source file but may not
  ;; work well on all major modes. If that's the case then it needs activation
  ;; on a per major-mode basis. However, if it needs to be disabled altogether,
  ;; please explain why for future reference.
  (setq c-auto-newline t)

  ;; Delete all trailing whitespace before saving
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
; (setq show-trailing-whitespace t)

  ;; The following disabled:
; (doxymacs-mode)                    ; turn doxymacs on
  )

(defun initialise-common-web ()
  (auto-fill-mode -1)
  (highlight-parentheses-mode)
  (setq tab-width       2
        c-basic-offset  2))

(defun initialise-c-c++()
  (local-set-key "\C-co" 'buftoggle)

  (c-toggle-auto-hungry-state 1)
  (c-toggle-auto-state -1)
  )

(defun initialise-sh ()
  (auto-fill-mode -1))

(defun initialise-c++ ()
  (setq comment-start "/* "
        comment-end   " */"))

(defun initialise-elisp ()
  (local-set-key (kbd "C-x C-k")  'do-eval-buffer)
  (turn-on-eldoc-mode))

(defun initialise-php ()
  (setq comment-start "/* "
        comment-end   " */")
  (eldoc-mode 1)
  (php-eldoc-enable))

(defun initialise-javascript ()
  (electric-indent-mode)
  (setq comment-start "/* "
        comment-end   " */")

  (local-set-key (kbd "M-a") 'c-beginning-of-statement)
  (local-set-key (kbd "M-e") 'c-end-of-statement))

(defun initialise-js2-mode ()
  "Add responsive logic to indent the current line whenever the
dot character (.) is typed on a continued expression."
  (local-set-key "."  '(lambda ()
                   (interactive)
                   (insert-char ?.)
                   (when (js2-continued-expression-p)
                     (indent-for-tab-command))))

  (local-set-key [f3]
                 '(lambda ()
                   (interactive)
                   (unless (and next-error-function
                                (not (string= (type-of (js2-next-error))
                                              "string")))
                     (flycheck-next-error)))))


(defun initialise-css ()
  (setq-default  css-electric-brace-behavior  nil
                 css-indent-offset            2)
  (auto-fill-mode -1)
  (eldoc-mode 1)
  (css-eldoc-enable)
  )

;; Defun invoked after pressing C-c C-e (see `initialise-elisp').
;; Evals the current buffer and displays a message.
(defun do-eval-buffer ()
  "Evaluate the current buffer.
This command should only be used with ELISP."
  (interactive)
  (cl-block inner
      (cond
       ((or (eq major-mode 'emacs-elisp-mode)
            (eq major-mode 'lisp-interaction-mode))
        (eval-buffer))
       (t (message "unsupported mode: %s" major-mode)
          (cl-return-from inner)))

    (message "buffer evaluated")))

;; Hooks for commonly used programming modes
(add-hook 'c-mode-common-hook         'initialise-common-programming)
(add-hook 'c-mode-common-hook         'initialise-c-c++)
(add-hook 'c++-mode-hook              'initialise-c++)
(add-hook 'lisp-mode-hook             'initialise-common-programming)
(add-hook 'emacs-lisp-mode-hook       'initialise-common-programming)
(add-hook 'sh-mode-hook               'initialise-common-programming)
(add-hook 'sh-mode-hook               'initialise-sh)
(add-hook 'makefile-mode-hook         'initialise-common-programming)
(add-hook 'log-edit-mode-hook         'initialise-common-text)
;; - web
(add-hook 'css-mode-hook              'initialise-common-programming)
(add-hook 'css-mode-hook              'initialise-css)
(add-hook 'js2-mode-hook              'initialise-common-programming)
(add-hook 'js2-mode-hook              'initialise-javascript)
(add-hook 'js2-mode-hook              'initialise-js2-mode)
(add-hook 'php-mode-hook              'initialise-php)
(add-hook 'html-mode-hook             'initialise-common-web)

;; Eldoc support
(add-hook 'emacs-lisp-mode-hook       'initialise-elisp)
(add-hook 'lisp-interaction-mode-hook 'initialise-elisp)

;;; init-programming.el ends here
