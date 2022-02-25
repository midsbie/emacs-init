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

;;; Documentation:
;;
;; * Configuration of `lsp-dart' including listing of all variables used by the
;;   package:
;;   https://github.com/emacs-lsp/lsp-dart

;;; Code:

(defun init/dart-mode()
  "Initialise `dart-mode'."
  ;; LSP will not work when using Flutter unless the `lsp-dart-flutter-sdk-dir'
  ;; variable is initialized with the path to the Flutter SDK.  This value is
  ;; easily obtained by running the command "flutter sdk-path" but care must be
  ;; taken to remove the extraneous newline char at the end.
  ;;
  ;;
  (unless (and (boundp 'lsp-dart-flutter-sdk-dir) lsp-dart-flutter-sdk-dir)
    (when (executable-find "flutter")
      (setq lsp-dart-flutter-sdk-dir
            (s-trim-right (shell-command-to-string "flutter sdk-path"))))))

(defun init/config/dart-mode ()
  "Configure `dart-mode' in the current buffer."
  ;; Start debugging session with `dap-debug'.
  (setq-local lsp-dart-dap-flutter-hot-reload-on-save t)
  (init/common-web-programming-mode))

(use-package dart-mode
  ;; Requiring the `s' package because `s-trim-right' is used above.
  :after (company flycheck s)
  :hook ((dart-mode . lsp)
         (dart-mode . init/config/dart-mode))
  :init
  (init/dart-mode))

;;; dart.el ends here
