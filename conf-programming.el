;; Includes
;; - from packages:
(load-library "multi-mode")
;; (load-library "doxymacs/doxymacs")

;; - using `load-path'
(load-library "buftoggle.git/buftoggle")
(load-library "builder.git/builder")
(load-library "vcx.git/vcx")

;; - using requires
(require 'yasnippet)
(require 'php-mode)

;; - omnis
(when (file-exists-p "/usr/local/share/omnis/clients/omniscient/omniscient.el")
  (add-to-list 'load-path "/usr/local/share/omnis/clients/")
  (load-library "omniscient/omniscient.el")
  (load-library "omniscient/cc-mode.el"))

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

;; Deactivate default PHP coding style so our coding style isn't overriden
(setq php-mode-coding-style nil)

;; Apparently there are potential issues with emacs attempting to set js2-mode
;; for .js files.  Switch to the old js-mode instead.
;; (setq auto-mode-alist (append '(("\\.js$"   . js-mode))
;;                               auto-mode-alist ))

;; VCx
(global-vcx-mode 1)

;; yasnippet
(setq yas/snippet-dirs '("~/.emacs.d/snippets"))
(yas/global-mode 1)

;; buftoggle
(add-to-list 'buftoggle-pairs-alist '("cxx" "hxx"))
(add-to-list 'buftoggle-pairs-alist '("hxx" "cxx"))

;; Setup hooks so major modes are customised.
(defun common-text-hook ()
  (enable-fci-mode)                 ; fill column indicator
  (auto-fill-mode)                  ; auto fill
  (flyspell-mode)                   ; turn spell check on
  (abbrev-mode -1)                  ; turn abbrev-mode off
  )

(defun common-initialise-programming ()
  (enable-fci-mode)                 ; fill column indicator
  (auto-fill-mode)                  ; auto fill
  (flyspell-prog-mode)              ; turn spell check for strings and comments
  (highlight-parentheses-mode)      ; turn on { } and ( ) highlighting
;; (setq c-auto-newline t)           ; set electricity on

;; (doxymacs-mode)                   ; turn doxymacs on
  (abbrev-mode -1)                  ; turn abbrev-mode off
  )

(defun common-initialise-web ()
  (auto-fill-mode -1)
  (highlight-parentheses-mode)
  (setq tab-width       2
        c-basic-offset  2))

(defun initialise-c-c++()
  (local-set-key "\C-co" 'buftoggle))

(defun initialise-sh-mode ()
  (setq sh-basic-offset 2
        sh-indentation 2))

(defun initialise-c++-mode ()
  (setq comment-start "/* "
        comment-end   " */"))

(defun on-eval-buffer ()
  (interactive)
  (eval-buffer)
  (message "buffer eval'ed"))

(defun initialise-elisp-mode ()
  (local-set-key (kbd "C-c C-e")  'on-eval-buffer)
  (turn-on-eldoc-mode))

(defun initialise-php-mode ()
  (flymake-php-load))

;; Hooks for commonly used programming modes
(add-hook 'c-mode-common-hook         'common-initialise-programming)
(add-hook 'common-c++-mode-hook       'initialise-c++-mode)
(add-hook 'c-mode-common-hook         'initialise-c-c++)
(add-hook 'common-c++-mode-hook       'initialise-c-c++)
(add-hook 'lisp-mode-hook             'common-initialise-programming)
(add-hook 'emacs-lisp-mode-hook       'common-initialise-programming)
(add-hook 'sh-mode-hook               'common-initialise-programming)
(add-hook 'sh-mode-hook               'initialise-sh-mode)
(add-hook 'makefile-mode-hook         'common-initialise-programming)
(add-hook 'log-edit-mode-hook         'initialise-common-text)
;; - web
(add-hook 'css-mode-hook              'common-initialise-programming)
(add-hook 'js-mode-hook               'common-initialise-programming)
(add-hook 'php-mode-hook              'common-initialise-web)
(add-hook 'php-mode-hook              'initialise-php-mode)
(add-hook 'html-mode-hook             'common-initialise-web)

;; Eldoc support
(add-hook 'emacs-lisp-mode-hook       'initialise-elisp-mode)
(add-hook 'lisp-interaction-mode-hook 'initialise-elisp-mode)
