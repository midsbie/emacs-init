(add-to-list 'load-path "/usr/share/emacs/common-lisp/auto-complete")
(add-to-list 'load-path "/usr/share/emacs/common-lisp/doxymacs")


;; Includes
(load-library "/usr/src/git-yasnippet/yasnippet")
(load-library "auto-complete-config")
(load-library "/usr/src/git-php-mode/php-mode")
(load-library "member-functions")

;; (load-library "conf-cedet");


;; member-functions
(autoload 'expand-member-functions
  "member-functions"
  "Expand C++ member function declarations" t)
(add-hook 'c++-mode-hook
          (lambda ()
            (local-set-key "\C-cx" #'expand-member-functions)))


;; yasnippet
(setq yas/snippet-dirs '("~/.emacs.d/snippets"
                         "/usr/src/git-yasnippet/snippets/"))
(yas/global-mode 1)


(defun common-text-hook ()
  (fci-mode)                        ; fill column indicator
  (auto-fill-mode)                  ; auto fill
  (flyspell-mode)                   ; turn spell check on
  )

(defun common-programming-hook ()

  (fci-mode)                        ; fill column indicator
  (auto-fill-mode)                  ; auto fill
  (flyspell-prog-mode)              ; turn spell check for strings and comments
  (highlight-parentheses-mode)      ; turn on { } and ( ) highlighting
  (follow-mode t)                   ; allow for easier editing of
                                    ; long buffers
  )

;; Hooks for commonly used programming modes
(add-hook 'c-mode-common-hook         'common-programming-hook)
(add-hook 'php-mode-hook              'common-programming-hook)
(add-hook 'css-mode-hook              'common-programming-hook)
(add-hook 'js-mode-hook               'common-programming-hook)
(add-hook 'lisp-mode-hook             'common-programming-hook)
(add-hook 'emacs-lisp-mode-hook       'common-programming-hook)
(add-hook 'sh-mode-hook               'common-programming-hook)
(add-hook 'makefile-mode-hook         'common-programming-hook)
(add-hook 'srecode-template-mode-hook 'common-programming-hook)
(add-hook 'log-edit-mode-hook         'common-atext-hook)


;; Hooks for lisp and elisp modes
(add-hook 'emacs-lisp-mode-hook       'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
