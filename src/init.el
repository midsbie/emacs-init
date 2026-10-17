;;; init.el --- Master emacs initialisation file  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; URL:
;; Keywords: tools
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:

;; ~/.emacs.d/init.el is a symlink to this file, as are early-init.el and
;; user-lisp/.  Settings that must apply before packages activate are in
;; early-init.el.
;;
;; The modules in config/, features/, modes/ and environment/ configure Emacs
;; as they load, in the order they are required below.  user-lisp/ is the
;; User Lisp directory: at startup Emacs byte-compiles the utilities in it and
;; registers their autoloads, so each loads the first time it is used.

;;; Code:

(defgroup init nil
  "Emacs initialisation mechanism.")

(defvar init/default-language-server-client 'eglot
  "Indicates which language server should be used by default when an
explicit entry does not exist in
`init/language-server-map-to-major-modes'.  Can be one of 'eglot,
'lsp or nil.  If nil, defaults to 'lsp.")

(defvar init/language-server-map-to-major-modes
  '(
    ;; Explicitly disabling language server for the major modes below
    ;; ------------------------------------------------------------------------
    (emacs-lisp-mode . nil)
    (lisp-mode . nil)
    (meson-mode . nil)
    (sql-mode . nil)
    ;; Explicitly specifying language server override for the major modes below
    ;; ------------------------------------------------------------------------
    (csharp-mode . eglot)
    (csharp-ts-mode . eglot)
    (vala-mode . eglot)
    ;; lsp-dart did not work particularly well under `dart-mode' in the past.
    ;; If this continues to be true, revert to eglot.
    ;; (dart-mode . eglot)
    )
  "List of mapping between major mode and a language server.  The
language server can be 'eglot, 'lsp or nil.  When nil, no
language server is used.")

(defvar-local init/inhibit-buffer-formatting nil
  "When non-nil, disable all automatic buffer formatting through LSP.")

(defvar init/buffer-format-handlers-alist
  '()
  "List of major modes for which to run a special function when saving the
buffer.  If none is specified for the mode associated with the buffer,
the LSP client's default formatting function is invoked.

Example:
  '(((js-mode js-ts-mode typescript typescript-ts-mode tsx-ts-mode) . my/special-formatter))
")

(defconst init/web-programming-modes
  '(js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode)
  "List of major modes that are related to web programming.")

(defvar init/beginning-of-statement-fn '(c-backward-sexp backward-sexp backward-sentence)
  "List containing the order of functions to execute when M-a is pressed.")

(defvar init/end-of-statement-fn '(c-forward-sexp forward-sexp forward-sentence)
  "List containing the order of functions to execute when M-e is pressed.")

(defvar init/compilation-jump-to-bottom t
  "Causes the caret to move to the bottom of the compilation buffer when t.")

(defconst init/defaults/fill-column 100
  "Default `fill-column' value.")

(defconst init/defaults/fill-column/narrow 80
  "Default narrow `fill-column' value.")

(defcustom init/theme 'my-gui-dark
  "Theme to load during GUI initialization.
Can be `my-gui-dark', `my-gui-light', or any other installed theme.
Terminal Emacs uses `wombat' with `my-term-dark' layered on top."
  :group 'init
  :type 'symbol)

;; Custom saves customizations here rather than in this file.  The file is
;; loaded last, below.
(setq custom-file (locate-user-emacs-file "custom.el"))

(message "[init] starting")
(add-hook 'after-init-hook 'init/post-init)

;; Install declared packages that are missing.  Built-in packages opt out with
;; `:ensure nil'.
;;
;; Ensuring by default, rather than marking external packages with `:ensure t',
;; makes a forgotten flag fail early and locally.  An external package missing
;; the flag still installs, whereas one missing `:ensure t' would load from the
;; existing `elpa/' and fail only on a fresh machine.  A built-in missing
;; `:ensure nil' is either a no-op or fails to install on every startup.
;;
;; `:ensure nil' is required only on built-ins that package.el does not list
;; (e.g. dired, nxml-mode, text-mode), but goes on every built-in so that no one
;; has to check.  It is unnecessary with `:vc', which ignores `:ensure'.
(setq use-package-always-ensure t)

;; Required by the `:diminish' keyword.
(use-package diminish)

;; Make the modules loadable and the themes in themes/ available to
;; `load-theme'.  `file-truename' resolves the symlink this file is loaded
;; through.
(let ((base (file-name-directory (file-truename (or load-file-name buffer-file-name)))))
  (dolist (dir '("config" "features" "modes" "environment"))
    (add-to-list 'load-path (expand-file-name dir base)))
  (setq custom-theme-directory (expand-file-name "themes/" base)))

;; Each module may rely on those required before it.
(require 'init-consts)
(require 'init-features)
(require 'init-keybindings)
(require 'init-log)
(require 'init-programming)
(require 'init-settings)

(require 'init-all-the-icons)
(require 'init-ansi-color)
(require 'init-avy)
(require 'init-consult)
(require 'init-copilot)
(require 'init-corfu)
(require 'init-desktop)
(require 'init-dired)
(require 'init-display-line-numbers)
(require 'init-eglot)
(require 'init-eldoc)
(require 'init-embark)
(require 'init-flycheck)
(require 'init-flymake)
(require 'init-format-all)
(require 'init-git-commit)
(require 'init-git-gutter)
(require 'init-goggles)
(require 'init-grep)
(require 'init-indent-bars)
(require 'init-lsp)
(require 'init-magit)
(require 'init-neotree)
(require 'init-orderless)
(require 'init-pet)
(require 'init-prettier)
(require 'init-project)
(require 'init-promptivel)
(require 'init-recentf)
(require 'init-server)
(require 'init-shell)
(require 'init-speedbar)
(require 'init-sqlformat)
(require 'init-tramp)
(require 'init-vertico)
(require 'init-visual-fill-column)
(require 'init-vterm)
(require 'init-which-key)
(require 'init-window-pulse)
(require 'init-with-editor)

;; The utilities in user-lisp/ need no `require'.  These three take effect only
;; once switched on.
(add-hook 'find-file-hook #'my/handle-large-file-hook)
(my/modeline-mode 1)
(add-hook 'after-change-major-mode-hook #'my/warn-when-root-visits-file)

(require 'init-cc)
(require 'init-conf)
(require 'init-csharp)
(require 'init-css)
(require 'init-dart)
(require 'init-go)
(require 'init-html)
(require 'init-jinja2)
(require 'init-js)
(require 'init-json)
(require 'init-lisp)
(require 'init-makefile)
(require 'init-markdown)
(require 'init-meson)
(require 'init-nxml)
(require 'init-org)
(require 'init-php)
(require 'init-python)
(require 'init-rust)
(require 'init-scss)
(require 'init-sh)
(require 'init-sql)
(require 'init-text)
(require 'init-typescript)
(require 'init-vala)
(require 'init-web)
(require 'init-yaml)

;; Load configurations specific to the environment
(require (if (display-graphic-p) 'init-x 'init-term))

;; Identity, mail and per-host settings, kept out of the repository, apply over
;; everything above.
(load (locate-user-emacs-file "personal.el") 'noerror 'nomessage)

(load custom-file 'noerror 'nomessage)

;; Print useful diagnostic messages
(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (message "Native compilation is available")
  (message "Native compilation is *not* available"))

(message "[init] done.")

;; --
(defun init/post-init ()
  "Perform post-init steps."
  ;; Show how long it took to initialise emacs after 1 idle second.
  (run-with-idle-timer 1 nil #'(lambda ()
                                 (message "init took %s" (emacs-init-time)))))

;;; init.el ends here
