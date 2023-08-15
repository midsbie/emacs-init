;;; flycheck.el --- Configures the flycheck feature

;; Copyright (C) 2015-2023  Miguel Guedes

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

;; You may need to add the following to .dir-local.el file if flycheck is
;; complaining that it cannot find eslint:
;;
;; ((typescript-ts-mode
;;   (eval . (setq-local flycheck-javascript-eslint-executable
;;                       (expand-file-name "node_modules/.bin/eslint"
;;                                         (locate-dominating-file default-directory ".dir-locals.el"))))))
;; OR for more than one major mode:
;;
;; ((nil . ((eval . (when (or (eq major-mode 'typescript-ts-mode)
;;                            (eq major-mode 'js-mode))
;;                   (setq-local flycheck-javascript-eslint-executable
;;                               (expand-file-name "node_modules/.bin/eslint"
;;                                                 (locate-dominating-file default-directory ".dir-locals.el"))))))))

;;; Code:

(defun init/flycheck/on-flyspell-mode()
  ;; Deactivate annoying correction of previous misspelled error when C-; is hit.
  (define-key flyspell-mode-map (kbd "C-;") nil))

;; Setup flycheck with repository-local eslint executable.
;;
;; Taken from:
;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;;
;; ... which was taken originally from:
;; https://emacs.stackexchange.com/q/21205/15089
;;
;; However, the original function was enhanced as to support mono-repositories
;; that host multiple projects via a meta-package manager such as Lerna and
;; where multiple `node_modules` directories may exist.
;;
;; This function now looks up the full directory tree looking for
;; `node_modules` and the eslint binary inside of it, until it hits the
;; filesystem root.
(defun init/flycheck/use-eslint-from-node-modules ()
  (let* ((curdir (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules")))
    (setq-local flycheck-javascript-eslint-executable nil)
    (while (and curdir (not flycheck-javascript-eslint-executable))
      (let ((eslint (expand-file-name "node_modules/.bin/eslint"
                                      curdir)))
        ;; Setting `flycheck-javascript-eslint-executable' terminates the loop
        ;; above.
        (if (and eslint (file-executable-p eslint))
            (setq-local flycheck-javascript-eslint-executable eslint)
          (setq curdir (file-name-directory (directory-file-name curdir))))))))

(defun init/flycheck ()
  "Configure `flycheck'."

  ;; Enable flycheck globally.
  (global-flycheck-mode 1)
  (load "flycheck-flow")

  ;; + in c/c++ modes
  (setq flycheck-cppcheck-checks '("all"))

  ;; + in `php-mode'
  (setq flycheck-phpmd-rulesets '("cleancode" "codesize" "controversial"
                                  "design" "naming" "unusedcode"))

  ;; Disable jshint since we prefer eslint.
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint
                          ;; Using `tide-mode' and `javascript-eslint'
                          typescript-tslint)))

  ;; + in `web-mode'
  ;; NOTE: htmltidy and csslint have been disabled since flycheck does not
  ;; support more than one linter per major mode; specifically, all the
  ;; defined linters per major mode are executed regardless of the actual
  ;; file type.
  ;;      (flycheck-add-mode 'html-tidy 'web-mode)
  ;;      (flycheck-add-mode 'css-csslint 'web-mode)

  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'javascript-eslint 'js-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'js-jsx-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  ;; IMPORTANT! Do not mess with the order in which the checkers are added
  ;; below.  Doing so will result in eslint being somehow overriden or
  ;; worse.
  ;; -- Flow
  (flycheck-add-mode 'javascript-flow 'web-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-flow-coverage)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)

  (add-hook 'flyspell-mode-hook 'init/flycheck/on-flyspell-mode)
  ;; Disabled because we are calling `init/add-node-modules-to-exec-path' as
  ;; part of the initialization sequence in `init/common-web-programming-mode',
  ;; which updates `exec-path' to point to the buffer's root "node_modules/.bin"
  ;; directory and should allow the eslint command to be found by
  ;; `flycheck-mode' if present.
  ;;
  ;; (add-hook 'flycheck-mode-hook 'init/flycheck/use-eslint-from-node-modules)

  ;; Show the error list in the bottom third of the present buffer's window.
  ;; The error list can be summoned via `M-x flycheck-list-errors` or
  ;; `C-c ! l`.
  ;;
  ;; Reference:
  ;; http://www.flycheck.org/en/latest/user/error-list.html#tune-error-list-display
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.15))))

(defun init/flycheck/config ()
  ;; Flymake's configuration turns flycheck-mode off automatically if eglot is
  ;; found to be running.
  )

(defun init/flycheck/chain-eslint-checker ()
  "Add javascript-eslint checker.

This function adds the javascript-eslint checker to the current
checkers after `lsp-diagnostics-mode' is loaded.  Attempting to
add the the checker before the load occurs fails with an error
because the checker does not exist."
  (when (and flycheck-mode lsp-diagnostics-mode)
    (flycheck-add-next-checker 'lsp 'javascript-eslint)))

(use-package flycheck
  :diminish "🐞"
  :hook ((flycheck-mode . init/flycheck/config)
         (lsp-diagnostics-mode . init/flycheck/chain-eslint-checker))

  ;; Not activating flycheck-popup-tip-mode because error messages frequently
  ;; do not respect boundaries of the window, often making it impossible to
  ;; read the full message.
  ;;
  ;; :hook ((flycheck-mode . flycheck-popup-tip-mode))
  :init (init/flycheck))

;;; flycheck.el ends here
