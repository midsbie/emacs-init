;;; init-modeline-tests.el --- Modeline activation regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'flymake)
(setq load-prefer-newer t)
(add-to-list 'load-path
             (expand-file-name "../src/user-lisp" (file-name-directory load-file-name)))
(require 'my-modeline)

(ert-deftest init/modeline-repeated-activation-preserves-state ()
  "Repeated activation owns one timer and restores the original format."
  (let ((original-format (default-value 'mode-line-format))
        (original-flymake-format flymake-mode-line-format)
        timers)
    (unwind-protect
        (dolist (format '(("original mode line") nil))
          (setq-default mode-line-format format)
          (my/modeline-mode 1)
          (push my/modeline--idle-timer timers)
          (let ((timer my/modeline--idle-timer))
            (my/modeline-mode 1)
            (push my/modeline--idle-timer timers)
            (should (eq timer my/modeline--idle-timer))
            (should (= 1 (cl-count timer timer-idle-list)))
            (should (= 1 (cl-count #'my/modeline--on-flymake-mode flymake-mode-hook)))
            (my/modeline-mode -1)
            (should-not (memq timer timer-idle-list)))
          (should (equal (default-value 'mode-line-format) format))
          (should (equal flymake-mode-line-format original-flymake-format))
          (should-not (memq #'my/modeline--on-flymake-mode flymake-mode-hook))
          (should-not (advice-member-p #'my/modeline--update-project-root
                                       'vc-refresh-state))
          (setq-default mode-line-format '("changed while disabled"))
          (my/modeline-mode -1)
          (should (equal (default-value 'mode-line-format) '("changed while disabled"))))
      (my/modeline-mode -1)
      (dolist (timer timers)
        (when (timerp timer) (cancel-timer timer)))
      (setq-default mode-line-format original-format))))

;;; init-modeline-tests.el ends here
