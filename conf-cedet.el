(load-file "/usr/src/bzr-cedet/cedet-devel-load.el")
;; (load-library "cedet")

(semantic-load-enable-excessive-code-helpers)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode t)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode t)

(defun setup-semantic-system-includes ()
  (semantic-add-system-include "/usr/include/"                      'c++-mode)
  (semantic-add-system-include "/usr/include/boost/"                'c++-mode)
  (semantic-add-system-include "/usr/include/glib-2.0/"             'c++-mode)
  (semantic-add-system-include "/usr/include/gtk-3.0/"              'c++-mode)
  (semantic-add-system-include "/usr/include/gtk-3.0/gtk/"          'c++-mode)
  (semantic-add-system-include "/usr/include/gtk-3.0/gdk/"          'c++-mode)
  (semantic-add-system-include "/usr/include/glibmm-2.4/"           'c++-mode)
  (semantic-add-system-include "/usr/include/gtkmm-3.0/"            'c++-mode)
  (semantic-add-system-include "/usr/include/libxml++-2.6/"         'c++-mode)
  (semantic-add-system-include "/usr/include/libxml++-2.6/libxml++" 'c++-mode)
  (semantic-add-system-include "/usr/include/qt4/"                  'c++-mode)

  (semantic-add-system-include "/usr/include/"                      'c-mode)
  (semantic-add-system-include "/usr/include/glib-2.0/"             'c-mode)
  (semantic-add-system-include "/usr/include/gtk-3.0/"              'c-mode)
  (semantic-add-system-include "/usr/include/gtk-3.0/gtk/"          'c-mode)
  (semantic-add-system-include "/usr/include/gtk-3.0/gdk/"          'c-mode)
  (semantic-add-system-include "/usr/include/libxml2/"              'c-mode)
  
  ;; (semantic-add-system-include "/usr/include/boost/"                'c-mode)
  ;; (semantic-add-system-include "/usr/include/glibmm-2.4/"           'c-mode)
  ;; (semantic-add-system-include "/usr/include/gtkmm-3.0/"            'c-mode)
  ;; (semantic-add-system-include "/usr/include/libxml++-2.6/"         'c-mode)
  ;; (semantic-add-system-include "/usr/include/libxml++-2.6/libxml++" 'c-mode)
  
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file
               "/usr/include/c++/4.7/x86_64-linux-gnu/bits/c++config.h")
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file
               "/usr/include/_G_config.h")
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file
               "/usr/include/python2.7/pyconfig.h")
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file
               "/usr/include/k3sconfig.h")
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file
               "/usr/include/freetype2/freetype/config/ftconfig.h")
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file
               "/usr/include/c++/4.7/x86_64-linux-gnu/32/bits/c++config.h")
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file
               "/usr/include/mysql/my_config.h")
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file
               "/usr/include/gtk-3.0/gdk/gdkconfig.h")
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file
               "/usr/include/qt4/Qt/qconfig.h")
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file
               "/usr/include/qt4/Qt/qconfig-dist.h")
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file
               "/usr/include/qt4/Qt/qglobal.h")
  
  (imenu-add-to-menubar "Semantic")
  )

(add-hook 'semantic-init-hooks 'setup-semantic-system-includes)

(semantic-mode 1)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

(defun cedet-hook ()
  (local-set-key "\C-c?"            'semantic-ia-complete-symbol)

  (local-set-key "\C-c>"            'semantic-complete-analyze-inline)
  (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)

  (local-set-key "\C-c="            'semantic-decoration-include-visit)

  (local-set-key "\C-cj"            'semantic-ia-fast-jump)
  (local-set-key "\C-cq"            'semantic-ia-show-doc)
  (local-set-key "\C-cs"            'semantic-ia-show-summary)
  (local-set-key "\C-cp"            'semantic-analyze-proto-impl-toggle)
  )

(add-hook 'c-mode-common-hook 'cedet-hook)
(add-hook 'lisp-mode-hook 'cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'cedet-hook)

(defun c-mode-cedet-hook ()
  ;; (local-set-key "." 'semantic-complete-self-insert)
  ;; (local-set-key ">" 'semantic-complete-self-insert)
  )

(add-hook 'c-mode-common-hook 'c-mode-cedet-hook)

(provide 'conf-cedet)
