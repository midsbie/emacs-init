;; (add-to-list 'load-path "/usr/share/emacs/common-lisp/doxymacs")


;; Includes
(load-library "/usr/src/git-yasnippet/yasnippet")
(load-library "/usr/src/git-php-mode/php-mode")
(load-library "/usr/src/git-buftoggle/buftoggle")
(load-library "./build")
;; (load-library "doxymacs/doxymacs")


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

;; Eldoc support
(add-hook 'emacs-lisp-mode-hook       'common-elisp-mode)
(add-hook 'lisp-interaction-mode-hook 'common-elisp-mode)