;; (add-to-list 'load-path "/usr/share/emacs/common-lisp/doxymacs")
(add-to-list 'load-path "/usr/src")

;; Includes
(load-library "git-yasnippet/yasnippet")
(load-library "git-buftoggle/buftoggle")
(load-library "multi-mode.el")
(load-library "git-php-mode/php-mode.el")
(load-library "./build")
(load-library "/usr/local/share/omnis/clients/omniscient/omniscient.el")
(load-library "/usr/local/share/omnis/clients/omniscient/cc-mode.el")
;; (load-library "doxymacs/doxymacs")


;; Setup support for web programming
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


;; yasnippet
(setq yas/snippet-dirs '("~/.emacs.d/snippets"))
(yas/global-mode 1)

;; buftoggle
(add-to-list 'buftoggle-pairs-alist '("cxx" "hxx"))
(add-to-list 'buftoggle-pairs-alist '("hxx" "cxx"))

(defun common-text-hook ()
  (fci-mode)                        ; fill column indicator
  (auto-fill-mode)                  ; auto fill
  (flyspell-mode)                   ; turn spell check on
  (abbrev-mode -1)                  ; turn abbrev-mode off
  )

(defun common-programming-hook ()
  (fci-mode)                        ; fill column indicator
  (auto-fill-mode)                  ; auto fill
  (flyspell-prog-mode)              ; turn spell check for strings and comments
  (highlight-parentheses-mode)      ; turn on { } and ( ) highlighting
;;   (setq c-auto-newline t)           ; set electricity on

;; (doxymacs-mode)                   ; turn doxymacs on
  (abbrev-mode -1)                  ; turn abbrev-mode off
  )

(defun common-web-hook ()
  (auto-fill-mode -1)
  (highlight-parentheses-mode)
  (setq tab-width       2
        c-basic-offset  2))

(defun c-c++-hook()
  (local-set-key "\C-co" 'buftoggle))

(defun sh-mode-hook ()
  (setq sh-basic-offset 2
        sh-indentation 2))

(defun c++-mode-hook ()
  (setq comment-start "/* "
        comment-end   " */"))

(defun on-eval-buffer ()
  (interactive)
  (eval-buffer)
  (message "buffer eval'ed"))

(defun common-elisp-mode ()
  (local-set-key (kbd "C-c C-e")  'on-eval-buffer)
  (turn-on-eldoc-mode))

;; Hooks for commonly used programming modes
(add-hook 'c-mode-common-hook         'common-programming-hook)
(add-hook 'c++-mode-hook              'c++-mode-hook)
(add-hook 'c-mode-common-hook         'c-c++-hook)
(add-hook 'c++-mode-hook              'c-c++-hook)
(add-hook 'css-mode-hook              'common-programming-hook)
(add-hook 'js-mode-hook               'common-programming-hook)
(add-hook 'php-mode-hook              'common-web-hook)
(add-hook 'html-mode-hook             'common-web-hook)
(add-hook 'lisp-mode-hook             'common-programming-hook)
(add-hook 'emacs-lisp-mode-hook       'common-programming-hook)
(add-hook 'sh-mode-hook               'common-programming-hook)
(add-hook 'sh-mode-hook               'sh-mode-hook)
(add-hook 'makefile-mode-hook         'common-programming-hook)
(add-hook 'srecode-template-mode-hook 'common-programming-hook)
(add-hook 'log-edit-mode-hook         'common-text-hook)

;; Eldoc support
(add-hook 'emacs-lisp-mode-hook       'common-elisp-mode)
(add-hook 'lisp-interaction-mode-hook 'common-elisp-mode)