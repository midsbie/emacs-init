;;; init-programming.el --- Programming mode configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)

(defvar init/eglot-disabled-modes
  '(emacs-lisp-mode lisp-mode meson-mode sql-mode)
  "Major modes in which Eglot should not start automatically.
Derived modes are also excluded.")

(defvar-local init/inhibit-buffer-formatting nil
  "When non-nil, disable automatic formatting through `init/maybe-format-buffer'.")

(defvar init/buffer-format-handlers-alist nil
  "Alist mapping major modes to functions called without arguments on save.
Each key is a mode symbol or a list of mode symbols.  The first matching
entry overrides the default formatting in `init/maybe-format-buffer'.")

(defconst init/web-programming-modes
  '(js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode)
  "List of major modes that are related to web programming.")

(defvar init/beginning-of-statement-fn
  '(c-backward-sexp backward-sexp backward-sentence)
  "Functions tried in order by `my/beginning-of-statement'.")

(defvar init/end-of-statement-fn
  '(c-forward-sexp forward-sexp forward-sentence)
  "Functions tried in order by `my/end-of-statement'.")

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
;; - This cannot be a string because `vala-mode' expects it to be an assoc list
;;   and errors out during initialization otherwise.
;; - Custom's type omits `vala-mode', so `setopt' would warn about this value.
(setq-default c-default-style    '((vala-mode . "default")
                                   (other . "default")))

;; Tree-sitter
;; - Apply all available syntax highlighting
(setopt treesit-font-lock-level 4)

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
  ;; Providing navigation between code blocks designated by curly brackets and parentheses.
  (local-set-key (kbd "C-x [") 'backward-up-list)
  (local-set-key (kbd "C-x ]") 'up-list)
  (local-set-key (kbd "C-x {") 'backward-list)
  (local-set-key (kbd "C-x }") 'forward-list)

  (local-set-key (kbd "M-a") #'my/beginning-of-statement)
  (local-set-key (kbd "M-e") #'my/end-of-statement)

  ;; Configure compile, unless the major mode already uses the key (e.g.
  ;; `python-shell-send-buffer', `sh-case').
  ;; `lookup-key' returns a number when `C-c' itself is unbound in the map.
  (let ((binding (and (current-local-map)
                      (lookup-key (current-local-map) (kbd "C-c C-c")))))
    (unless (and binding (not (numberp binding)))
      (local-set-key (kbd "C-c C-c") 'compile)))

  (abbrev-mode -1)                  ; turn abbrev-mode off
  (subword-mode 1)

  ;; `global-auto-revert-mode' is enabled but some major modes disable it, for
  ;; some reason.  One such mode is `python-ts-mode'.
  (auto-revert-mode 1)

  ;; Delete all trailing whitespace before saving.  -100 argument makes it so
  ;; this particular hook runs before any other, which fixes a number of issues
  ;; in some modes where the server's buffer formatting would run ahead of
  ;; 'delete-trailing-whitespace but the whitespace would not be removed for
  ;; some reason.  Modes in which this was observed include `sh-mode'.
  (add-hook 'before-save-hook 'delete-trailing-whitespace -100 t) ; local hook
  (setq-local show-trailing-whitespace t)

  ;; Start Eglot for the active programming mode.
  (init/run-language-server))

(defun init/common-nonweb-programming-mode ()
  "Perform initialisation of aspects common to all programming-related modes."
  ;; NOTE: we are explicitly disabling `flyspell-mode' since it suffers from
  ;; very annoying, intrusive issues that actually impede development.
  ;;(flyspell-prog-mode)            ; turn spell check for strings and comments

  ;; `c-auto-newline' was previously disabled and done so for a reason. Have
  ;; just tested it and seemed fine whilst editing a C source file but may not
  ;; work well on all major modes. If that's the case then it needs activation
  ;; on a per major-mode basis. However, if it needs to be disabled altogether,
  ;; make sure to document reason for future reference.
  ;;
  ;; Update: disabled because it turns out to be too intrusive when editing
  ;; code.  It (mostly) works fine when writing new code though.
  (setq c-auto-newline nil)

  (init/common-programming-mode))


(defun init/common-web-programming-mode ()
  "Initialise modes related to web development."
  (setq-local tab-width       2)
  (setq-local c-basic-offset  2)
  (setq-local fill-column     init/defaults/fill-column)

  ;; Add node_modules path to `exec-path' to enable:
  ;;
  ;; - `prettier-mode' to work
  ;;
  ;; - Eglot to find the TypeScript language server for Typescript
  ;;   source files.
  ;;
  ;; [31Oct22] The `add-node-modules-path' package is no longer used because
  ;; it does not take into account the fact that mono-repositories often have
  ;; multiple "node_modules" directories.  The custom function
  ;; `init/add-node-modules-to-exec-path' was crafted to take this into account
  ;; when looking for a repository's root "node_modules" directory.
  (init/add-node-modules-to-exec-path)

  (init/common-programming-mode)

  ;; Default `compile' to the package's tests unless the mode chose a command.
  ;; File and directory local values apply after this hook and take precedence.
  (when (string= compile-command (default-value 'compile-command))
    (setq-local compile-command
                (concat (if (and (executable-find "yarn")
                                 (locate-dominating-file default-directory "yarn.lock"))
                            "yarn"
                          "npm") " run test"))))

(defun init/find-node-modules ()
  "Find path to node_modules directory.

Attempts to determine the path to the repository's topmost node_modules
directory by recursively traversing all the directories of the current
buffer."
  (when-let* ((path (my/locate-topmost-file (or (buffer-file-name) default-directory)
                                            "node_modules"))
              (full-path (expand-file-name "node_modules" path)))
    (when (file-directory-p full-path)
      full-path)))

(defun init/add-node-modules-to-exec-path ()
  "Add node_modules/.bin directory to buffer-local `exec-path' when present.

Searches for node_modules/.bin in the buffer's directory tree and adds
it to the buffer-local `exec-path' if found."
  (let* ((p (init/find-node-modules))
         (binp (and p (expand-file-name ".bin" p))))
    (when (and binp (file-directory-p binp))
      (make-local-variable 'exec-path)
      (add-to-list 'exec-path binp))))

(defun init/is-web-programming-mode-p ()
  "Return t if current `major-mode' is a web programming mode."
  (or (memq major-mode init/web-programming-modes)
      (and
       (eq major-mode 'web-mode)
       (or
        (string= web-mode-content-type "js")
        (string= web-mode-content-type "jsx")))))

(defun init/run-eslint-autofix-if-applicable ()
  (when (init/is-web-programming-mode-p)
    (init/run-eslint-autofix)))

(defun init/run-eslint-autofix ()
  "Apply ESLint fixes to the visited file and refresh the buffer.

Meant to run after saving, so that imports are organized as the
repository's settings require.  ESLint runs synchronously on the file on
disk, so a modified buffer is refused rather than reverted.  The first
ESLint on the buffer's `exec-path' runs from the buffer's directory, so
the nearest installation and configuration apply.  Signal an error when
ESLint is missing or fails to run."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (when (buffer-modified-p)
    (user-error "Save the buffer before applying ESLint fixes"))
  (let ((eslint (or (executable-find "eslint" t)
                    (user-error "ESLint not found")))
        (file (file-local-name buffer-file-name)))
    (with-temp-buffer
      ;; The temporary buffer inherits `default-directory', from which
      ;; ESLint 9 looks up its configuration.  ESLint exits with 1 when
      ;; problems remain after fixing.
      (let ((status (process-file eslint nil t nil "--fix" file)))
        (unless (memq status '(0 1))
          (error "ESLint failed with status %s: %s"
                 status (string-trim (buffer-string))))))
    (unless (verify-visited-file-modtime)
      (revert-buffer t t t))))

(defun init/run-language-server ()
  "Start Eglot unless the current mode is in `init/eglot-disabled-modes'."
  (unless (apply #'derived-mode-p init/eglot-disabled-modes)
    (eglot-ensure)))

(defun init/buffer-formatting-inhibited-p ()
  "Returns t if master circuit breaker preventing buffer formatting is
enabled."
  (and (boundp 'init/inhibit-buffer-formatting) init/inhibit-buffer-formatting))

(defun init/maybe-format-buffer ()
  "Conditionally format buffer.

Formats the current buffer if formatting isn't inhibited.  Runs a custom
formatter function, if one is specified in
`init/buffer-format-handlers-alist', otherwise formats through Eglot
if neither `prettier-mode' nor `format-all-mode' are enabled."
  (unless (init/buffer-formatting-inhibited-p)
    (if-let* ((formatter (init/get-format-buffer-function-for-mode major-mode)))
        (funcall formatter)
      (unless (or
               (and (boundp 'prettier-mode) prettier-mode)
               (and (boundp 'format-all-mode) format-all-mode))
        ;; Eglot loads lazily; its predicate is not autoloaded.
        (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
          (eglot-format-buffer))))))

(defun init/get-format-buffer-function-for-mode (mode)
  "Get the save function for the current buffer's major mode."
  (let ((pair
         (cl-some (lambda (pair)
                    (when (or (and (listp (car pair))
                                   (memq mode (car pair)))
                              (eq mode (car pair)))
                      pair))
                  init/buffer-format-handlers-alist)))
    (and pair (cdr pair))))

(provide 'init-programming)

;;; init-programming.el ends here
