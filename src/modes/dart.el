;;; dart.el --- Configures `dart-mode'

;; Copyright (C) 2022  Miguel Guedes

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
;; * Unfortunately the `dap-debug' and `lsp-dart-dap--flutter-hot-reload'
;;   commands seem to consistently fail to work again once the first Chrome
;;   instance hosting a debugging session crashes.  It wasn't clear why this
;;   happened and sometimes not even restarting Emacs fixed it.  Since
;;   hot-reloading is a super important develoment aid, support was added here
;;   and should occur whenever the following conditions are true:
;;
;;     1. a buffer was saved
;;
;;     2. the major mode of the buffer just saved is `dart-mode'
;;
;;     3. the buffer is contained in a Git repository
;;
;;     4. a file with the name ".flutter-debug.pid" (customizable by
;;        `init/dart-mode/flutter-pid-file') exists in the repository's root
;;        directory and it contains a valid PID pointing to a running flutter
;;        process
;;
;;  (4) above requires running flutter as given below:
;;
;;    $ flutter run --debug --hot --pid-file=${init/dart-mode/flutter-pid-file}

;;; Documentation:
;;
;; * Configuration of `lsp-dart' including listing of all variables used by the
;;   package:
;;   https://github.com/emacs-lsp/lsp-dart

;;; Code:

(defvar init/dart-mode/flutter-pid-file ".flutter-debug.pid")

(defun init/dart-mode()
  "Initialise `dart-mode'."

  (setq
   ;; Explicitly enable automatic code formatting on buffer save.
   lsp-dart-enable-sdk-formatter   t
   lsp-dart-line-length            init/defaults/fill-column
   lsp-dart-main-code-lens         nil
   lsp-dart-test-code-lens         nil
   ;; This may be necessary to improve performance.
   ;; Ref: https://gitter.im/emacs-lsp/lsp-mode?at=5eefb9d3c223cc536a294315
   lsp-dart-flutter-widget-guides  nil)

  ;; LSP will not work when using Flutter unless the `lsp-dart-flutter-sdk-dir'
  ;; variable is initialized with the path to the Flutter SDK.  This value is
  ;; easily obtained by running the command "flutter sdk-path" but care must be
  ;; taken to remove the extraneous newline char at the end.
  (unless (and (boundp 'lsp-dart-flutter-sdk-dir) lsp-dart-flutter-sdk-dir)
    (when (executable-find "flutter")
      (setq lsp-dart-flutter-sdk-dir
            (s-trim-right (shell-command-to-string "flutter sdk-path"))))))

(defun init/config/dart-mode ()
  "Configure `dart-mode' in the current buffer."
  ;; Start debugging session with `dap-debug'.
  (setq-local lsp-dart-dap-flutter-hot-reload-on-save t)
  (init/common-web-programming-mode)

  ;; Changing the default setting of 1 because company mode in some
  ;; circumstances seems to be implicated in occasional (sometimes too frequent)
  ;; micro-stutters wnen the list of candidates returned by the LSP server is
  ;; too large.  Notably it was happening fairly consistently when editing
  ;; specific Dart/Flutter source files.  Since the increase to 2,
  ;; microstuttering seems to have reduced.
  (setq-local company-minimum-prefix-length 2)

  (setq-local company-backends
              '(company-capf company-dabbrev company-dabbrev-code))

  ;; Support for hot-reloading whenever a Dart/Flutter source file is saved.
  ;; This requires flutter to be run in debug mode, with hot-reloading enabled
  ;; (the default behavior), and for a PID file to exist in the repository's
  ;; root directory:
  ;;
  ;;   $ flutter run --debug --hot --pid-file=${init/dart-mode/flutter-pid-file}
  (add-hook 'after-save-hook #'init/dart-mode/maybe-hotreload nil t))

(defun init/dart-mode/maybe-hotreload ()
  "Maybe hot-reload Flutter debugging session."
  (ignore-errors
    (let* ((root-dir (vc-git-root (buffer-file-name)))
           (pid-file-name (concat root-dir init/dart-mode/flutter-pid-file)))
      (when (and root-dir (file-exists-p pid-file-name))
        (signal-process (string-to-number (with-temp-buffer
                                            (insert-file-contents pid-file-name)
                                            (buffer-string))) 'SIGUSR1)))))

(use-package dart-mode
  ;; Requiring the `s' package because `s-trim-right' is used above.
  :after (company flycheck s)
  :hook (
         ;;(dart-mode . lsp)
         (dart-mode . eglot-ensure)
         (dart-mode . init/config/dart-mode))
  :init
  (init/dart-mode))

;;; dart.el ends here
