;;; init-grep-tests.el --- Grep state regression tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'grep)
(add-to-list 'load-path
             (expand-file-name "../src/features" (file-name-directory load-file-name)))
(require 'init-grep)

(ert-deftest init/git-grep-restores-state-on-error ()
  (let ((grep-use-null-device t)
        (seen 'unset))
    (cl-letf (((symbol-function 'grep)
               (lambda (_command)
                 (setq seen grep-use-null-device)
                 (user-error "Command aborted"))))
      (should-error (my/git-grep "git grep needle") :type 'user-error))
    (should (null seen))
    (should grep-use-null-device)))

(ert-deftest init/git-grep-restores-state-on-success ()
  (let ((grep-use-null-device t)
        (seen 'unset))
    (cl-letf (((symbol-function 'grep)
               (lambda (_command) (setq seen grep-use-null-device))))
      (my/git-grep "git grep needle"))
    (should (null seen))
    (should grep-use-null-device)))

;;; init-grep-tests.el ends here
