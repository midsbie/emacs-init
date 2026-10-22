;;; init-dart-mode.el --- Configure Dart mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Hot-reload Flutter whenever the following conditions are true:
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
  "Hot-reload the Flutter session named by the repository's PID file.
Signal nothing unless the file holds a positive integer.  The PID of a
remote buffer's session is signalled on that buffer's host."
  (ignore-errors
    (let* ((root-dir (vc-git-root (buffer-file-name)))
           (pid-file-name (concat root-dir init/dart-mode/flutter-pid-file)))
      (when (and root-dir (file-exists-p pid-file-name))
        (let* ((contents (string-trim (with-temp-buffer
                                        (insert-file-contents pid-file-name)
                                        (buffer-string))))
               (pid (and (string-match-p "\\`[0-9]+\\'" contents)
                         (string-to-number contents))))
          ;; Zero and negative PIDs would signal process groups.
          (when (and pid (> pid 0))
            (signal-process pid 'SIGUSR1 (file-remote-p root-dir))))))))

(use-package dart-mode
  :hook (dart-mode . init/dart-mode/enable))

(provide 'init-dart-mode)

;;; init-dart-mode.el ends here
