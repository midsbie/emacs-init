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

(defvar init/eglot-disabled-modes
  '(emacs-lisp-mode lisp-mode meson-mode sql-mode)
  "Major modes in which Eglot should not start automatically.
Derived modes are also excluded.")

(defvar-local init/inhibit-buffer-formatting nil
  "When non-nil, disable all automatic buffer formatting through Eglot.")

(defvar init/buffer-format-handlers-alist
  '()
  "List of major modes for which to run a special function when saving the
buffer.  If none is specified for the mode associated with the buffer,
Eglot's default formatting function is invoked.

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
(setopt custom-file (locate-user-emacs-file "custom.el"))

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
(setopt use-package-always-ensure t)

;; Make the modules loadable and the themes in themes/ available to
;; `load-theme'.  `file-truename' resolves the symlink this file is loaded
;; through.
(let ((base (file-name-directory (file-truename (or load-file-name buffer-file-name)))))
  (dolist (dir '("config" "features" "modes" "environment"))
    (add-to-list 'load-path (expand-file-name dir base)))
  (setopt custom-theme-directory (expand-file-name "themes/" base)))

;; Each module may rely on those required before it.
;; Core settings are independent of package configuration.
(require 'init-consts)
(require 'init-keybindings)
(require 'init-log)
(require 'init-programming)
(require 'init-settings)

;; Diminish must be available before declarations that use its keyword.
(require 'init-diminish)
(require 'init-ace-window)
(require 'init-autorevert)
(require 'init-browse-kill-ring)
(require 'init-buffer-move)
(require 'init-dabbrev)
(require 'init-erc)
(require 'init-display-fill-column-indicator)
(require 'init-highlight-parentheses)
(require 'init-flyspell)
(require 'init-gcmh)
(require 'init-marginalia)
(require 'init-recursion-indicator)
(require 'init-savehist)
(require 'init-simple)
(require 'init-subword)
(require 'init-all-the-icons)
(require 'init-all-the-icons-dired)
(require 'init-all-the-icons-completion)
(require 'init-all-the-icons-ibuffer)
(require 'init-ansi-color)
(require 'init-avy)
(require 'init-consult)
(require 'init-copilot)
(require 'init-corfu)
(require 'init-css-eldoc)
(require 'init-desktop)
(require 'init-dired)
(require 'init-dired-x)
(require 'init-display-line-numbers)
(require 'init-eglot)
(require 'init-eldoc)
(require 'init-embark)
(require 'init-embark-consult)
(require 'init-flymake)
(require 'init-flymake-ruff)
(require 'init-flymake-eslint)
(require 'init-format-all)
(require 'init-git-commit)
(require 'init-git-gutter-fringe)
(require 'init-git-gutter)
(require 'init-goggles)
(require 'init-grep)
(require 'init-indent-bars)
(require 'init-magit)
(require 'init-neotree)
(require 'init-orderless)
(require 'init-pet)
(require 'init-prettier)
(require 'init-project)
(require 'init-promptivel)
(require 'init-recentf)
(require 'init-ruff-format)
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
(require 'init-xclip)

;; The utilities in user-lisp/ need no `require'.  These three take effect only
;; once switched on.
;; Mode-selection alists run after explicit mode cookies and shebangs; intercept
;; selection itself so large files skip language hooks in every case.
(advice-add 'set-auto-mode :before-until #'my/handle-large-file-hook)
(my/modeline-mode 1)
(add-hook 'after-change-major-mode-hook #'my/warn-when-root-visits-file)

(require 'init-cc-mode)
(require 'init-csharp-mode)
(require 'init-css-mode)
(require 'init-dart-mode)
(require 'init-go-ts-mode)
(require 'init-html-ts-mode)
(require 'init-jinja2-mode)
(require 'init-js)
(require 'init-json-ts-mode)
(require 'init-elisp-mode)
(require 'init-make-mode)
(require 'init-markdown-mode)
(require 'init-meson-mode)
(require 'init-nxml-mode)
(require 'init-org)
(require 'init-php-ts-mode)
(require 'init-python)
(require 'init-rust-ts-mode)
(require 'init-sh-script)
(require 'init-sql)
(require 'init-text-mode)
(require 'init-typescript-ts-mode)
(require 'init-vala-mode)
(require 'init-web-mode)
(require 'init-yaml-ts-mode)

;; Initialize each display type when it first appears, including daemon clients.
(defun init/configure-frame (frame)
  "Load the environment configuration for FRAME's display type."
  (with-selected-frame frame
    (require (if (display-graphic-p frame) 'init-x 'init-term))))

(add-hook 'after-make-frame-functions #'init/configure-frame)
(init/configure-frame (selected-frame))

;; Identity, mail and per-host settings, kept out of the repository, apply over
;; everything above.
(load (locate-user-emacs-file "personal.el") 'noerror 'nomessage)

(load custom-file 'noerror 'nomessage)

;; Print useful diagnostic messages
(unless (and (fboundp 'native-comp-available-p)
             (native-comp-available-p))
  (message "Native compilation is NOT available"))

(message "[init] done.")

;; --
(defun init/post-init ()
  "Perform post-init steps."
  ;; Show how long it took to initialise emacs after 1 idle second.
  (run-with-idle-timer 1 nil #'(lambda ()
                                 (message "init took %s" (emacs-init-time)))))

;;; init.el ends here
