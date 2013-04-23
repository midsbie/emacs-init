;; (add-to-list 'load-path "/usr/share/emacs/common-lisp/auto-complete")
(add-to-list 'load-path "/usr/share/emacs/common-lisp/doxymacs")


;; Includes
(load-library "conf-cedet")
(load-library "/usr/src/git-yasnippet/yasnippet")
;; (load-library "auto-complete-config")
(load-library "/usr/src/git-php-mode/php-mode")
(load-library "member-functions")
(load-library "./build")
(load-library "doxymacs/doxymacs")


;; member-functions
(autoload 'expand-member-functions
  "member-functions"
  "Expand C++ member function declarations" t)
(add-hook 'c++-mode-hook
          (lambda ()
            (local-set-key "\C-cx" #'expand-member-functions)))


;; yasnippet
(setq yas/snippet-dirs '("~/.emacs.d/snippets"))
(yas/global-mode 1)

;; auto-complete
(setq ac-auto-show-menu    0.1)
(setq ac-menu-height       30)
(setq ac-ignore-case       "No")
;; (ac-config-default)


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
  
;;   (add-to-list 'ac-dictionary-directories
;;                "/usr/share/emacs/common-lisp/auto-complete/ac-dict")
  
  (doxymacs-mode)                   ; turn doxymacs on
  (abbrev-mode -1)                  ; turn abbrev-mode off
  )

(defun sh-mode-hook ()
  (setq sh-basic-offset 2
        sh-indentation 2))

(defun c++-mode-hook ()
  (setq comment-start "/*"
        comment-end   "*/"))
  

;; Hooks for commonly used programming modes
(add-hook 'c-mode-common-hook         'common-programming-hook)
(add-hook 'php-mode-hook              'common-programming-hook)
(add-hook 'css-mode-hook              'common-programming-hook)
(add-hook 'js-mode-hook               'common-programming-hook)
(add-hook 'lisp-mode-hook             'common-programming-hook)
(add-hook 'emacs-lisp-mode-hook       'common-programming-hook)
(add-hook 'sh-mode-hook               'common-programming-hook)
(add-hook 'sh-mode-hook               'sh-mode-hook)
(add-hook 'makefile-mode-hook         'common-programming-hook)
(add-hook 'srecode-template-mode-hook 'common-programming-hook)
(add-hook 'log-edit-mode-hook         'common-text-hook)
(add-hook 'c++-mode                   'c++-mode-hook)

;; Hooks for lisp and elisp modes
(add-hook 'emacs-lisp-mode-hook       'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
