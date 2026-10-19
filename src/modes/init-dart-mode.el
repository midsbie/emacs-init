;;; init-dart-mode.el --- Configures `dart-mode'  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Miguel Guedes

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
;;
;;; Code:

(defvar init/dart-mode/flutter-pid-file ".flutter-debug.pid")

(defun init/dart-mode/enable ()
  "Configure `dart-mode' in the current buffer."
  (init/common-nonweb-programming-mode)
  ;; `dart-mode' sets `fill-column' to 80; keep the configured default.
  (setq-local fill-column init/defaults/fill-column)

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
  :hook (dart-mode . init/dart-mode/enable))

(provide 'init-dart-mode)

;;; init-dart-mode.el ends here
