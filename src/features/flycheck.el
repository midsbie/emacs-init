;;; flycheck.el --- Configures the flycheck feature

;; Copyright (C) 2015-2020  Miguel Guedes

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

;;; Code:


(defvar my/flycheck-buffer-last 0
  "Last time `'flycheck-buffer' ran.")

(defvar my/flycheck-buffer-time-between 2.5
  "Minimum time that must elapse between invocations of `flycheck-buffer'.")

(defvar my/flycheck-buffer-timer nil
  "Timer for next scheduled invocation of `flycheck-buffer'.")

(defvar my/flycheck-buffer--func nil)
(defvar my/flycheck-buffer--args nil)

(defun my/flycheck-buffer (orig &rest args)
  "Function advice around `flycheck-buffer'.
This is an attempt at making `flycheck' more performant with
`lsp', especially when editing C# source files.  We make it so
`flycheck-buffer' is executed at most once every number of
seconds specified by `my/flycheck-buffer-time-between'.  It is
not executed if company mode's popup is active to prevent visual
corruption.  Likewise, it is not executed if flycheck is
running."
  (interactive)
  (when (timerp my/flycheck-buffer-timer)
    (cancel-timer my/flycheck-buffer-timer)
    (setq my/flycheck-buffer-timer nil))
  (let* ((seconds-from-last (- (float-time) my/flycheck-buffer-last))
         (seconds-to-next (- my/flycheck-buffer-time-between seconds-from-last)))
    ;; Must copy ORIG and ARGS or they won't be available in the timer lambda func.
    (setq my/flycheck-buffer--func orig
          my/flycheck-buffer--args args)
    (run-with-idle-timer (max seconds-to-next 0) nil
                         '(lambda ()
                             (setq my/flycheck-buffer-last (float-time)
                                   my/flycheck-buffer-timer nil)
                             (unless (or (flycheck-running-p) (company--active-p))
                               (apply my/flycheck-buffer--func my/flycheck-buffer--args))))))

(defun init/config/flycheck ()
  "Configure `flycheck'."

  ;; Enable flycheck globally.
  (global-flycheck-mode 1)
  (load "flycheck-flow")

  ;; Advice removal may not be needed but here for correctness anyway.
  (advice-remove 'flycheck-buffer #'my/flycheck-buffer)
  (advice-add 'flycheck-buffer :around #'my/flycheck-buffer)

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

  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)

  ;; IMPORTANT! Do not mess with the order in which the checkers are added
  ;; below.  Doing so will result in eslint being somehow overriden or
  ;; worse.
  ;; -- Flow
  (flycheck-add-mode 'javascript-flow 'web-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-flow-coverage)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)

  (add-hook 'flyspell-mode-hook 'init/flycheck/on-flyspell-mode)
  (add-hook 'flycheck-mode-hook 'init/flycheck/use-eslint-from-node-modules)

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
      (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js"
                                      curdir)))
        ;; Setting `flycheck-javascript-eslint-executable' terminates the loop
        ;; above.
        (if (and eslint (file-executable-p eslint))
            (setq-local flycheck-javascript-eslint-executable eslint)
          (setq curdir (file-name-directory (directory-file-name curdir))))))))

(use-package flycheck
  :hook ((flycheck-mode . flycheck-popup-tip-mode))
  :init
  (init/config/flycheck))

;;; flycheck.el ends here
