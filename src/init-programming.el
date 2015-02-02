;; init-programming.el --- Initialisation of programming related features
;;
;; Copyright (C) 2014 Miguel Guedes
;;
;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; URL: 
;; Keywords: init
;; Version: 1.0
;;
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
;;
;; Comments:
;; 
;;


;; ----- Includes
;; - from packages:
(load-library "multi-mode")
;; (load-library "doxymacs/doxymacs")

;; - using `load-path'
(load-library "buftoggle.git/buftoggle")
(load-library "builder.git/src/builder")
(load-library "vcx.git/src/vcx")

;; - from ELPA:
(require 'yasnippet)
(require 'php-mode)
(require 'highlight-parentheses)
(require 'js2-mode)
(require 'flymake-jshint)

;; Various settings
(setq change-log-default-name "CHANGELOG")

;; set up js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default js2-basic-offset  2)


;; load cc-mode
(autoload 'awk-mode "cc-mode" nil t)    ; Unsure why we want this?

;; - omnis
(when (file-exists-p "/usr/local/share/omnis/clients/omniscient/omniscient.el")
  (add-to-list 'load-path "/usr/local/share/omnis/clients/")
  (load-library "omniscient/omniscient.el")
  (load-library "omniscient/cc-mode.el"))

;; ----- Web
;; DISABLED: Multi web mode
;; (require 'multi-web-mode)
;; 
;; (setq mweb-default-major-mode 'html-mode)
;; (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;;                   (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
;;                   (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
;; (setq mweb-filename-extensions '("php" "htm" "html" "ctp"
;;                                  "phtml" "php4" "php5"))
;; (multi-web-global-mode 1)

;; Multi mode: setup support for web programming
(setq magic-mode-alist
      (append magic-mode-alist '(("<\\?"               . setup-web-mode))))
(setq magic-mode-alist
      (append magic-mode-alist '(("../usr/bin/env php" . setup-web-mode))))
(setq magic-mode-alist
      (append magic-mode-alist '(("<html"              . setup-web-mode))))
(setq magic-mode-alist
      (append magic-mode-alist '(("<head"              . setup-web-mode))))
(setq magic-mode-alist
      (append magic-mode-alist '(("<body"              . setup-web-mode))))
(setq magic-mode-alist
      (append magic-mode-alist '(("<\\!DOCTYPE html"   . setup-web-mode))))

(defun setup-web-mode()
  (multi-mode 1
              'html-mode
              '("<\?"           php-mode)
              '("<\?php"        php-mode)
              '("\?>"           html-mode)
              '("<script"       js-mode)
              '("</script>"     html-mode)
              '("<style"        css-mode)
              '("</style>"      html-mode)
              ;;'("\{\"" json-mode)       ; DISABLED
              ))


;; ----- Various settings
;; VCx
(global-vcx-mode 1)

;; yasnippet
(setq yas/snippet-dirs '("~/.emacs.d/snippets"))
(yas/global-mode 1)

;; buftoggle
(add-to-list 'buftoggle-pairs-alist '("cxx" "hxx" "hpp" "hh" "h"))
(add-to-list 'buftoggle-pairs-alist '("hxx" "cxx" "cpp" "cc"))

;; scss-mode
(setq scss-sass-command "scss")

;; default settings
(setq-default
 ;; c-mode and derived modes default style and tabs
 c-default-style    "linux"
 c-basic-offset     2
 tab-width          2
 indent-tabs-mode   nil
 ;; javascript
 js-indent-level    2
 ;; sh
 sh-basic-offset    2
 sh-indentation     2
 php-mode-coding-style      'drupal
 php-lineup-cascaded-calls  t)

;; Set default product when on `sql-mode'
(setq sql-product 'mysql)

;; -----  Setup customisation of major modes.
(defun initialise-common-programming ()
  (enable-fci-mode)                 ; fill column indicator
  (auto-fill-mode)                  ; auto fill
  (flyspell-prog-mode)              ; turn spell check for strings and comments
  (highlight-parentheses-mode)      ; turn on { } and ( ) highlighting
;; (setq c-auto-newline t)             ; set electricity on

;; (doxymacs-mode)                     ; turn doxymacs on
  (abbrev-mode -1)                  ; turn abbrev-mode off
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
  (c-set-offset 'innamespace 0)         ; set indentation inside namespaces to
                                        ; nil
  )

(defun initialise-sh ()
  (auto-fill-mode -1))

(defun initialise-c++ ()
  (setq comment-start "/* "
        comment-end   " */"))

(defun initialise-elisp ()
  (local-set-key (kbd "C-c C-e")  'do-eval-buffer)
  (turn-on-eldoc-mode))

(defun initialise-php ()
  (flymake-php-load))

(defun initialise-javascript ()
  (flymake-jshint-load)
  (electric-indent-mode)
  (setq comment-start "/* "
        comment-end   " */")

  (local-set-key (kbd "M-a") 'c-beginning-of-statement)
  (local-set-key (kbd "M-e") 'c-end-of-statement))

(defun initialise-js2-mode ()
  ;; Following snippet adds responsive logic that indents the current line
  ;; whenever the dot character (.) is typed on a continued expression.
  (local-set-key "."
                 '(lambda ()
                   (interactive)
                   (insert-char ?.)
                   (when (js2-continued-expression-p)
                     (indent-for-tab-command))))

  (local-set-key [f3]
                 '(lambda ()
                   (interactive)
                   (unless (and next-error-function
                                (not (string= (type-of (js2-next-error)) "string")))
                     (flymake-goto-next-error)))))
  

(defun initialise-css ()
  (setq-default  css-electric-brace-behavior  nil
                 css-indent-offset            2)
  (auto-fill-mode -1)
  (flymake-css-load))

;; Defun invoked after pressing C-c C-e (see `initialise-elisp').
;; Evals the current buffer and displays a message.
(defun do-eval-buffer ()
  (interactive)
  (eval-buffer)
  (message "buffer evaluated"))

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
