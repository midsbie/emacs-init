;;; init-csharp-mode.el --- Configure C# modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; C# buffers use csharp-ls through Eglot.  Navigation into decompiled
;; assemblies (`csharp:/' sources) is configured in features/init-eglot.el.
;;
;; features/init-format-all.el enables formatting only when the project
;; contains .clang-format.

;;; Code:

(defun init/csharp-mode/toggle-ts ()
  "Toggle between `csharp-mode' and `csharp-ts-mode'.
`csharp-ts-mode' is preferred but has indentation edge cases that
can be worked around by temporarily switching to `csharp-mode'."
  (interactive)
  (pcase major-mode
    ('csharp-mode (csharp-ts-mode))
    ('csharp-ts-mode (csharp-mode))
    (_ (user-error "Not in a C# buffer"))))

(defun init/csharp-mode/enable ()
  "Set up a buffer in `csharp-mode' or `csharp-ts-mode'."
  ;; Teach `M-q' to reflow `"..." + "..."' string concatenations (only in the
  ;; tree-sitter mode).  Chain to the mode's own filler for everything else so
  ;; comment filling keeps working.  See `user-lisp/my-csharp-strings.el'.
  (when (treesit-parser-list nil 'c-sharp)
    (unless (eq fill-paragraph-function #'my/csharp-fill-paragraph)
      (setq-local my/csharp--prev-fill-paragraph-function fill-paragraph-function))
    (setq-local fill-paragraph-function #'my/csharp-fill-paragraph)

    ;; `csharp-ts-mode' has no indent rule for expression bodies, so a `=>'
    ;; line, or the expression after a trailing `=>', is left where it is in
    ;; properties, indexers and accessors (column 0 after a newline), and
    ;; gets the declaration's indentation in methods.  Indent both one level
    ;; from the declaration, as `csharp-mode' does.  Still missing upstream
    ;; as of Emacs 31.1.
    (treesit-simple-indent-add-rules
     'c-sharp
     '(((node-is "arrow_expression_clause") parent-bol csharp-ts-indent-offset)
       ((parent-is "arrow_expression_clause") parent-bol csharp-ts-indent-offset))
     :before)

    ;; Likewise a parameter on its own line is left where it is: the mode's
    ;; rule names the Java grammar's `formal_parameters' node instead of C#'s
    ;; `parameter_list', so it never fires (bug#67709).  Add the rule last, so
    ;; the mode's rule for a closing `)' still wins.
    (treesit-simple-indent-add-rules
     'c-sharp
     '(((parent-is "parameter_list") parent-bol csharp-ts-indent-offset))
     :after))

  (init/common-nonweb-programming-mode)

  ;; Hide test sources from `M-?' by default.  See `features/init-consult.el'.
  (setq-local init/consult/xref-references-initial-input "!Test "))

(use-package csharp-mode
  :ensure nil
  :mode ("\\.cs\\'" . csharp-ts-mode)
  :hook ((csharp-mode csharp-ts-mode) . init/csharp-mode/enable)
  :bind (:map csharp-mode-map
         ("C-c C-t" . init/csharp-mode/toggle-ts)
         :map csharp-ts-mode-map
         ("C-c C-t" . init/csharp-mode/toggle-ts)))

(provide 'init-csharp-mode)

;;; init-csharp-mode.el ends here
