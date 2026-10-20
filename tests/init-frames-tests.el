;;; init-frames-tests.el --- Frame initialization regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'use-package)

(defconst init-test/source-directory
  (expand-file-name "../src/" (file-name-directory load-file-name)))

(ert-deftest init/graphical-client-after-terminal-startup ()
  "Initialize a graphical client even when Emacs started on a terminal."
  (let ((after-init-hook nil)
        (after-change-major-mode-hook nil)
        (find-file-hook nil)
        (after-make-frame-functions nil)
        (features (remq 'init-frames features))
        (graphic nil)
        initialized
        (init-file (expand-file-name "init.el" init-test/source-directory))
        (original-load (symbol-function 'load))
        (original-require (symbol-function 'require)))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) graphic))
              ((symbol-function 'require)
               (lambda (feature &rest args)
                 (if (and (string-prefix-p "init-" (symbol-name feature))
                          (not (memq feature '(init-consts init-startup init-frames))))
                     (progn
                       (when (memq feature '(init-x init-term))
                         (push feature initialized))
                       t)
                   (apply original-require feature args))))
              ((symbol-function 'load)
               (lambda (file &rest args)
                 (unless (member (file-name-nondirectory file)
                                 '("personal.el" "custom.el"))
                   (apply original-load file args)))))
      (funcall original-load init-file nil t)
      (should (equal initialized '(init-term)))
      (setq graphic t)
      (run-hook-with-args 'after-make-frame-functions (selected-frame))
      (should (equal initialized '(init-x init-term))))))

;;; init-frames-tests.el ends here
