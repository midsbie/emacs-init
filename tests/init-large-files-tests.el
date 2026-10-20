;;; init-large-files-tests.el --- Large-file policy tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(setq load-prefer-newer t)
(add-to-list 'load-path
             (expand-file-name "../src/user-lisp" (file-name-directory load-file-name)))
(require 'my-large-files)

(ert-deftest init/large-files-skip-language-hooks ()
  "Large files bypass mode hooks even with an explicit mode cookie."
  (dolist (header '("" ";; -*- mode: emacs-lisp; -*-\n"))
    (let* ((calls 0)
           (emacs-lisp-mode-hook (list (lambda () (setq calls (1+ calls)))))
           (handler (lambda (&rest _) (my/handle-large-file-hook))))
      (advice-add 'set-auto-mode :before-until handler)
      (unwind-protect
          (with-temp-buffer
            (buffer-enable-undo)
            (insert header (make-string (1+ (* 1024 1024)) ?x))
            (setq buffer-file-name "/tmp/triage-large.el")
            (normal-mode t)
            (should (= calls 0))
            (should (eq major-mode 'fundamental-mode))
            (should buffer-read-only)
            (should (eq buffer-undo-list t))
            (normal-mode t)
            (should (= calls 0)))
        (advice-remove 'set-auto-mode handler)))))

(ert-deftest init/small-files-keep-language-mode ()
  (let* ((calls 0)
         (emacs-lisp-mode-hook (list (lambda () (setq calls (1+ calls)))))
         (handler (lambda (&rest _) (my/handle-large-file-hook))))
    (advice-add 'set-auto-mode :before-until handler)
    (unwind-protect
        (with-temp-buffer
          (buffer-enable-undo)
          (insert ";; small file\n")
          (setq buffer-file-name "/tmp/triage-small.el")
          (normal-mode t)
          (should (= calls 1))
          (should (eq major-mode 'emacs-lisp-mode))
          (should-not buffer-read-only)
          (should-not (eq buffer-undo-list t)))
      (advice-remove 'set-auto-mode handler))))

;;; init-large-files-tests.el ends here
