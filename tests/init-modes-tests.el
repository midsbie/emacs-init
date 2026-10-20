;;; init-modes-tests.el --- Major-mode regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'package)

(defconst init-test/modes-root
  (expand-file-name "../" (file-name-directory load-file-name)))
(let ((package-user-dir (expand-file-name "elpa" init-test/modes-root))
      (package-quickstart nil))
  (package-initialize))
(require 'use-package)
(setq use-package-always-ensure nil)
(dolist (dir '("modes" "user-lisp"))
  (add-to-list 'load-path (expand-file-name (concat "src/" dir) init-test/modes-root)))

(ert-deftest init/css-buffer-preserves-configured-keymap ()
  (require 'init-css-mode)
  (require 'css-mode)
  (let ((binding (lookup-key css-mode-map "}")))
    (cl-letf (((symbol-function 'init/common-nonweb-programming-mode) #'ignore))
      (with-temp-buffer
        (css-mode)))
    (should (eq (lookup-key css-mode-map "}") binding))))

(ert-deftest init/web-buffer-preserves-configured-keymap ()
  (require 'init-web-mode)
  (require 'web-mode)
  (let ((binding (lookup-key web-mode-map ".")))
    (cl-letf (((symbol-function 'init/common-web-programming-mode) #'ignore))
      (with-temp-buffer
        (web-mode)))
    (should (eq (lookup-key web-mode-map ".") binding))))

;;; init-modes-tests.el ends here
