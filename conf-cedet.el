(add-to-list 'load-path "/usr/src/bzr-cedet")
(add-to-list 'load-path "/usr/src/bzr-cedet/contrib")

;; includes
(load-library "cedet-devel-load")
(load-library "contrib/cedet-contrib-load")
(load-library "contrib/eassist")
;; (load-library "/usr/src/async/auto-complete-clang-async")

 
;; eassist
(add-to-list 'eassist-header-switches '("cxx" "hxx"))
(add-to-list 'eassist-header-switches '("hxx" "cxx"))


;; setup Srecoder global mode
(global-srecode-minor-mode)


;; (add-to-list 'semantic-default-submodes
;;              'global-semantic-idle-scheduler-mode t)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
;; (add-to-list 'semantic-default-submodes
;;              'global-semantic-idle-completions-mode t)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode t)

(defun init-semantic-hook ()
  ;; (semantic-add-system-include "/usr/include/glib-2.0/"             'c++-mode)
  ;; (semantic-add-system-include "/usr/include/gtk-3.0/"              'c++-mode)

  ;; (semantic-add-system-include "/usr/include/boost/"                'c++-mode)
  (semantic-add-system-include "/usr/include/glibmm-2.4/"           'c++-mode)
  (semantic-add-system-include "/usr/include/gtkmm-3.0/"            'c++-mode)
  ;; (semantic-add-system-include "/usr/include/giomm-2.4/"            'c++-mode)
  ;; (semantic-add-system-include "/usr/include/gdkmm-3.0/"            'c++-mode)
  (semantic-add-system-include "/usr/include/libxml++-2.6/"         'c++-mode)

  ;; (semantic-add-system-include "/usr/include/glib-2.0/"             'c-mode)
  ;; (semantic-add-system-include "/usr/include/gtk-3.0/"              'c-mode)
  ;; (semantic-add-system-include "/usr/include/libxml2/"              'c-mode)
  (semantic-add-system-include "/usr/include/glibmm-2.4/"           'c-mode)
  (semantic-add-system-include "/usr/include/gtkmm-3.0/"            'c-mode)
  (semantic-add-system-include "/usr/include/giomm-2.4/"            'c-mode)
  (semantic-add-system-include "/usr/include/gdkmm-3.0/"            'c-mode)
  (semantic-add-system-include "/usr/include/libxml++-2.6/"         'c-mode)
  
;;  (imenu-add-to-menubar "Semantic")
  )

(add-hook 'semantic-init-hooks 'init-semantic-hook)

(defun common-cedet-hook ()
  (local-set-key [\C-S-iso-lefttab] 'semantic-ia-complete-symbol)
;;  (local-set-key [(control tab)]    'ac-complete-clang-async)
  (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  (local-set-key (kbd "C-.")        'semantic-complete-analyze-inline)

  (local-set-key "\C-c="            'semantic-decoration-include-visit)

  (local-set-key "\C-cj"            'semantic-ia-fast-jump)
  (local-set-key "\C-cq"            'semantic-ia-show-doc)
  (local-set-key "\C-cs"            'semantic-ia-show-summary)
  (local-set-key "\C-cp"            'semantic-analyze-proto-impl-toggle)
  
  (local-set-key "\C-co"            'eassist-switch-h-cpp)
  (local-set-key "\C-cm"            'eassist-list-methods)
  
  ;; Add font-lock for doxymacs support
;;   (add-hook 'font-lock-mode-hook
;;             (lambda()
;;               (doxymacs-font-lock)))
  )


(defun ac-clang-async-hook ()
  (when (or (string= major-mode "c-mode")
            (string= major-mode "c++-mode"))
    (setq ac-clang-complete-executable "/usr/src/async/clang-complete")
    (setq ac-clang-cflags (read-lines "~/.emacs.d/clang-async.conf"))
    (ac-clang-launch-completion-process))
  )

(defun ac-cedet-hook ()
  (if (or (string= major-mode "php-mode")
          (string= major-mode "js-mode")
          (string= major-mode "css-mode"))
      (add-to-list 'ac-sources 'ac-source-dictionary)))


;; (add-hook 'c-mode-common-hook         'ac-clang-async-hook)
;; (add-hook 'auto-complete-mode-hook    'ac-cedet-hook)

(add-hook 'c-mode-common-hook         'common-cedet-hook)
(add-hook 'php-mode-hook              'common-cedet-hook)
(add-hook 'css-mode-hook              'common-cedet-hook)
(add-hook 'js-mode-hook               'common-cedet-hook)
(add-hook 'lisp-mode-hook             'common-cedet-hook)
(add-hook 'emacs-lisp-mode-hook       'common-cedet-hook)
(add-hook 'sh-mode-hook               'common-cedet-hook)
(add-hook 'makefile-mode-hook         'common-cedet-hook)
(add-hook 'srecode-template-mode-hook 'common-cedet-hook)

(semantic-mode -1)

(custom-set-variables
 '(semantic-idle-scheduler-idle-time 0.6)
 '(semantic-decoration-styles
   (quote (
           ("semantic-tag-boundary")
           ("semantic-decoration-on-includes" . t)
           ("semantic-decoration-on-protected-members")
           ("semantic-decoration-on-private-members")))))
