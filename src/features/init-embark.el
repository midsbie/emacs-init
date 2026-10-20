;;; init-embark.el --- Configure embark  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

;; Embark exposes no public API for its outline sentinel.  Read its constant
;; rather than duplicating the value, so copying follows the package's format.
(defvar embark-collect--outline-string)

(defun init/embark-collect/filter-buffer-substring (text)
  "Remove Embark's invisible outline sentinel from copied TEXT.

Embark prefixes group headings with `embark-collect--outline-string', whose
character is outside the Unicode range.  It is invisible in the collect buffer,
but copying includes it and encodes it as an invalid five-byte UTF-8 sequence,
which clipboard consumers commonly render as five replacement characters."
  (string-replace embark-collect--outline-string "---" text))

(defun init/embark-collect/enable ()
  "Configure copying from an Embark Collect buffer."
  ;; Filter the returned text rather than replacing the buffer's existing
  ;; substring filter, preserving any filtering installed by another mode.
  (add-function :filter-return
                (local 'filter-buffer-substring-function)
                #'init/embark-collect/filter-buffer-substring))

(use-package embark
  :bind
  (("C-." . embark-act)                 ; pick some comfortable binding
   ("C-;" . embark-dwim)                ; good alternative: M-.
   ("<f1> B" . embark-bindings))        ; alternative for `describe-bindings'
                                        ; which retains <f1> b chord

  :hook
  (embark-collect-mode . init/embark-collect/enable)

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(provide 'init-embark)

;;; init-embark.el ends here
