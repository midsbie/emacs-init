;;; programming.el --- Programming-related configuration

;; Copyright (C) 2015-2023  Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; Global programming settings
;; --
;; Own custom default style
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
                                  ;;                               space-before-funcall
                                  compact-empty-funcall))
               (c-hanging-semi&comma-criteria . ((lambda () 'stop)))
               (c-offsets-alist . ((innamespace           . 0)
                                   (statement-block-intro . +)
                                   (knr-argdecl-intro     . 0)
                                   (substatement-open     . 0)
                                   (substatement-label    . 0)
                                   (label                 . 0)
                                   (statement-cont        . +)))))

;; TODO: Investigate how to make the following work with
;; `c-hanging-semi&comma-criteria above.  Tried and failed to get it to work and
;; in the end simply disabled newline on semi.
;;
;;                . (c-semi&comma-no-newlines-for-oneline-inliners
;;                   c-semi&comma-inside-parenlist
;;                   c-semi&comma-no-newlines-before-nonblanks))

;; No clean-ups.  May prove useful in chaotically formatted code.
;; NOTE: don't forget to also turn off `c-auto-newline'!
(c-add-style "default-bland"
             '("default"
               (c-cleanup-list . nil)))

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

;; Default settings
(setq-default c-default-style    "default" ; own default style
              tab-width          2
              indent-tabs-mode   nil
              js-indent-level    2)

;; Tree-sitter
;; - Apply all available syntax highlighting
(setq-default treesit-font-lock-level 4)

;; Set the extra indentation before a substatement (e.g. the opening brace in
;; the consequent block of an if statement) to 0 (instead of '+)
;; Ref: https://stackoverflow.com/a/3956173
(c-set-offset 'substatement-open 0)

;; Hooks
(add-hook 'minibuffer-mode-hook 'init/disable-electricity)
(add-hook 'conf-mode-hook 'init/disable-electric-indent)

;; Defuns
;; --
(defun init/disable-electricity()
  "Disable electric enhancements."
  (electric-pair-local-mode -1)
  (electric-layout-local-mode -1)
  (init/disable-electric-indent))

(defun init/disable-electric-indent()
  "Disable electric indentation behaviour."
  (electric-indent-local-mode -1))

(defun init/common-programming-mode ()
  ;; NOTE: we are explicitly disabling `flyspell-mode' since it suffers from
  ;; very annoying, intrusive issues that actually impede development.
  (setq-local programming-buffer t)

  ;; Providing navigation between code blocks designated by curly brackets and parentheses.
  (local-set-key (kbd "C-x [") 'backward-up-list)
  (local-set-key (kbd "C-x ]") 'up-list)
  (local-set-key (kbd "C-x {") 'backward-list)
  (local-set-key (kbd "C-x }") 'forward-list)

  (local-set-key (kbd "M-a") #'my/beginning-of-statement)
  (local-set-key (kbd "M-e") #'my/end-of-statement)

  ;; Configure compile
  (local-set-key (kbd "C-c C-c") 'compile)

  (highlight-parentheses-mode)      ; turn on { } and ( ) highlighting
  (abbrev-mode -1)                  ; turn abbrev-mode off
  (subword-mode 1)

  ;; Delete all trailing whitespace before saving
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t) ; local hook
  (setq-local show-trailing-whitespace t)

  ;; Attempt to run the language server for the active programming mode and
  ;; enable tree-sitter.
  (init/run-language-server))

(defun init/common-nonweb-programming-mode ()
  "Perform initialisation of aspects common to all programming-related modes."
  (auto-fill-mode)                  ; auto fill

;;(flyspell-prog-mode)              ; turn spell check for strings and comments
;;(ac-flyspell-workaround)          ; this defun must be executed to prevent
                                    ; flyspell from messing with
                                    ; auto-complete-mode

  ;; `c-auto-newline' was previously disabled and done so for a reason. Have
  ;; just tested it and seemed fine whilst editing a C source file but may not
  ;; work well on all major modes. If that's the case then it needs activation
  ;; on a per major-mode basis. However, if it needs to be disabled altogether,
  ;; make sure to document reason for future reference.
  ;;
  ;; Update: disabled because it turns out to be too intrusive when editing
  ;; code.  It (mostly) works fine when writing new code though.
  (setq c-auto-newline nil)

  ;; The following disabled:
  ;; (doxymacs-mode)                    ; turn doxymacs on
  (init/common-programming-mode))


(defun init/common-web-programming-mode ()
  "Initialise modes related to web development."

  (auto-fill-mode -1)                   ; unclear why this is enabled in common
                                        ; non-web

  (setq-local fill-column init/defaults/fill-column)
  (setq-local tab-width   2)
  (setq-local c-basic-offset  2)

  ;; Add node_modules path to `exec-path' to enable:
  ;;
  ;; - `prettier-mode' to work
  ;;
  ;; - LSP to correctly identify the `ts-ls' language server for Typescript
  ;;   source files.
  ;;
  ;; Note that this requires an appropriate `use-package' invocation to load
  ;; the `add-node-modules-path' package.  Refer to ./use-package.el for
  ;; further infor.
  ;;
  ;; [31Oct22] This is now disabled because `add-node-modules-path' does not
  ;; take into account the fact that mono-repositories often have multiple
  ;; "node_modules" directories.  The custom function
  ;; `init/add-node-modules-to-exec-path' was crafted to take this into account
  ;; when looking for a repository's root "node_modules" directory.
  ;;
  ;; (add-node-modules-path)
  (init/add-node-modules-to-exec-path)

  (init/common-programming-mode)
  (add-hook 'after-save-hook #'init/run-eslint-autofix-if-applicable nil t)

  ;; For some reason preemptively setting the fill column above is now not
  ;; taking hold in the following modes: `tide-mode', `js-mode'.
  ;;
  ;; Leave this statement at the end of this defun or it may not run.
  (setq-local fill-column init/defaults/fill-column)
  (run-with-idle-timer .1 nil #'(lambda ()
                                  (setq-local fill-column init/defaults/fill-column)))

  ;; Provide default command for `compile'
  (setq-local compile-command "yarn run test"))

(defun init/find-node-modules ()
  "Find path to node_modules directory.

This defun attempts to determine the path to the repository's
node_modules directory by recursively traversing all the
directories of the current buffer and returning the path to the
last node_modules seen."
  (let* ((p (locate-dominating-file (or (buffer-file-name) default-directory)
                                    "node_modules"))
         (last nil))
    (while (and p (not (string= p "/")))
    (setq p (expand-file-name p))
      (when (and (file-directory-p (expand-file-name "node_modules" p)))
        (setq last p))
      (setq p (file-name-directory (directory-file-name p))))
    last))

(defun init/add-node-modules-to-exec-path ()
  "Add special node_modules/.bin directory to `exec-path', if found
in the buffer's directory tree."
  (let* ((p (init/find-node-modules))
         (binp (and p (expand-file-name "node_modules/.bin" p))))
    (when (and binp (file-directory-p binp))
      (make-local-variable 'exec-path)
      (add-to-list 'exec-path binp))))

(defun init/run-eslint-autofix-if-applicable ()
  (when (is-web-programming-mode-p)
    (init/run-eslint-autofix)))

(defun init/run-eslint-autofix ()
  "Apply eslint auto-fixes after buffer save.

This function applies eslint's auto-fixes automatically to a
buffer after it has been saved.  The main motivating factor here
is to make organization of imports as per the repository's
settings to be less of a pain point.

It is unclear whether there will be performance issues from
running the eslint tool in blocking mode."
  (interactive)
  (when-let ((project (project-current)))
    (let* ((root (expand-file-name (car (project-roots project)))))
      (shell-command-to-string (format "%s/node_modules/.bin/eslint --fix %s" root buffer-file-name))
      (revert-buffer t t t))))

(defun init/run-language-server ()
  ;; The following comments in modes/js.el kept for posteriority.
  ;; ---
  ;; Was activating flow minor mode previously:
  ;;
  ;;   (when (flycheck-flow--predicate)
  ;;     (flow-minor-mode 1))
  ;;
  ;; Then support transitioned to LSP for Flow source files and to eglot for all
  ;; other source files:
  ;;
  ;;   (cond
  ;;    ((flycheck-flow--predicate)
  ;;     (lsp-deferred))
  ;;    (t
  ;;     (eglot-ensure)))
  ;;
  ;; Now exclusively using eglot.
  (if (not init/prefer-eglot-lsp-client)
      (cl-case major-mode
        ('emacs-lisp-mode
         nil)
        (t
         (lsp-deferred)))
    (eglot-ensure)))

;;; programming.el ends here
