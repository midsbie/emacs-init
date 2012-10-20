;; (load-file "/usr/src/cedet-1.1/common/cedet.el")
(load-library "cedet")

;; (semantic-load-enable-excessive-code-helpers)
(setq semantic-default-submodes
      '(global-semantic-idle-scheduler-mode
        global-semanticdb-minor-mode
        global-semantic-idle-summary-mode
        global-semantic-idle-completions-mode
        global-semantic-highlight-func-mode
        global-semantic-decoration-mode
        global-semantic-stickyfunc-mode
        global-semantic-mru-bookmark-mode))
(semantic-mode)
;;(require 'semantic-ia)
;;(require 'semantic-gcc)

(semantic-add-system-include "/usr/include/boost/"          'c++-mode)
(semantic-add-system-include "/usr/include/glib-2.0/"       'c++-mode)
(semantic-add-system-include "/usr/include/glib-2.0/"       'c-mode)
(semantic-add-system-include "/usr/include/glibmm-2.4/"     'c++-mode)
(semantic-add-system-include "/usr/include/gtk-3.0/"        'c++-mode)
(semantic-add-system-include "/usr/include/gtk-3.0/"        'c-mode)
(semantic-add-system-include "/usr/include/gtkmm-3.0/"      'c++-mode)
(semantic-add-system-include "/usr/include/libxml++-2.6/"   'c++-mode)

(defun my-semantic-hook ()
  (imenu-add-to-menubar "Semantic"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

(defun cedet-hook ()
  ;; (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)

  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)

  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  ;; (local-set-key (kbd "C-c <left>") 'semantic-tag-folding-fold-block)
  ;; (local-set-key (kbd "C-c <right>") 'semantic-tag-folding-show-block)

  ;; (add-to-list 'ac-sources 'ac-source-semantic)
  )

(add-hook 'c-mode-common-hook 'cedet-hook)
(add-hook 'lisp-mode-hook 'cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'cedet-hook)

(defun c-mode-cedet-hook ()
  ;; (local-set-key "." 'semantic-complete-self-insert)
  ;; (local-set-key ">" 'semantic-complete-self-insert)
  ;; (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  ;; (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  ;; (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)

  ;; (add-to-list 'ac-sources 'ac-source-etags)
  ;; (add-to-list 'ac-sources 'ac-source-gtags)
  (setq ac-sources '(ac-source-semantic-raw))
  )

(add-hook 'c-mode-common-hook 'c-mode-cedet-hook)

(provide 'conf-cedet)
