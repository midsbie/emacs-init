;;; init.el --- Master emacs initialisation file

;; Copyright (C) 2014-2024 Miguel Guedes

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

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'cl-lib)
  (require 'package))

(defgroup init nil
  "Emacs initialisation mechanism."
  :group 'init)

(defvar init/path-base (file-name-directory (or load-file-name (buffer-file-name)))
  "Absolute path to Emacs' init `src´ directory.
If nil, `default-directory' is used instead.")

(defvar init/dirs-load '("config" "features" "extra-features" "modes")
  "Directories to automatically load.
Contains directories to automatically load as part of the
initialisation process.  Directories must be relative to
`init/path-base'.")

(defvar init/dir-packages (expand-file-name init/path-base "/elpa")
  "Directory to packages.
Absolute path to directory containing packages managed by the
`package' feature.")

(defvar init/open-at-startup nil
  "List containing files to visit when Emacs finishes loading.

Files are only visited if the server hasn't yet been started.")

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

(defconst init/defaults/light-theme nil
  "When nil, the default Emacs theme is turned into a dark theme.")

(defun init/inclusion-path (path)
  (concat (file-name-as-directory init/path-base) path))

(message "[init] starting")
(add-hook 'after-init-hook 'init/post-init)
(run-at-time "3 sec" nil #'init/delayed-init)

;; Load our compatibility library
(load (init/inclusion-path "./compat"))

;; Setup and load ELPA packages (and others) first and foremost
(setq package-user-dir (concat init/dir-packages "../elpa/src"))
;; Avoid loading byte-compiled packages that are older than the source file.
(setq load-prefer-newer t)

;; Setup package archives
(setq package-archives
      '(("GNU ELPA" . "https://elpa.gnu.org/packages/")
        ("MELPA stable" . "https://stable.melpa.org/packages/")
        ("MELPA unstable" . "https://melpa.org/packages/")
        ("ORG" . "http://orgmode.org/elpa/"))
      package-archive-priorities
      '(
        ;; MELPA stable disabled as not all packages support stable channel
        ;; ("MELPA stable" . 10)
        ("MELPA unstable" . 9)
        ("ORG" . 6)
        ("GNU ELPA" . 5)))

(package-initialize)
(require 'use-package)

;; Taken verbatim from: http://www.emacswiki.org/emacs/DotEmacsModular
;; Must be interpreted before loading begins below.
(defun my/load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY without the extension."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat (file-name-as-directory directory) path))
           (isdir (car (cdr element)))
           (ignore (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore))
        (my/load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))

;; Now safe to load all ELISP source files in directories specified in
;; `init/dirs-load'
(when load-file-name
  (dolist (loading init/dirs-load)
    (let ((dir-loading (or (concat init/path-base
                                   loading))))
      (message "Loading ELISP files in: %s" dir-loading)
      (my/load-directory dir-loading))))


;; Load configurations specific to the environment
(load (init/inclusion-path
       (or (and (display-graphic-p) "environment/x")
           "environment/term")))

;; Load files in `init/open-at-startup' list after a short delay so as to enable
;; the user to mutate the `init/open-at-startup' list.
(dolist (file init/open-at-startup)
  (if (not (file-exists-p file))
      (message "%s" (concat "error: file does not exist: " file))
    (find-file file)
    (with-current-buffer (current-buffer)
      (when (eq major-mode 'org-mode)
        (org-shifttab 2)))
    (other-window 1)))

;; Print useful diagnostic messages
(if (and (fboundp 'native-comp-available-p)
       (native-comp-available-p))
  (message "Native compilation is available")
(message "Native complation is *not* available"))

(if (functionp 'json-serialize)
  (message "Native JSON is available")
(message "Native JSON is *not* available"))

(message "[init] done.")

;; --
(defun init/post-init ()
  "Perform post-init steps."
  ;; Show how long it took to initialise emacs after 1 idle second.
  (run-with-idle-timer 1 nil #'(lambda ()
                                 (message "init took %s" (emacs-init-time)))))

(defun init/delayed-init ()
  "Delayed initializations steps."
  ;; Disable 'buffer * still has clients' message shown when killing buffers
  ;; spawned by emacsclient.
  ;;
  ;; NOTE: for some reason the call to remove-hook needs to take place a few
  ;; seconds after emacs has launched.
  (remove-hook 'kill-buffer-query-functions
               'server-kill-buffer-query-function))

;;; init.el ends here
