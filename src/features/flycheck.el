;;; flycheck.el --- Configures the flycheck feature

;; Copyright (C) 2015  Miguel Guedes

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

(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-flow)

     ;; + in c/c++ modes
     (setq flycheck-cppcheck-checks        '("all")
           flycheck-c/c++-clang-executable "true") ; disable clang since we use
                                                   ; omnis

     ;; + in `php-mode'
     (setq flycheck-phpmd-rulesets '("cleancode" "codesize" "controversial"
                                     "design" "naming" "unusedcode"))

     ;; Disable jshint since we prefer eslint.
     (setq-default flycheck-disabled-checkers
                   (append flycheck-disabled-checkers '(javascript-jshint)))

     ;; + in `web-mode'
     ;; NOTE: htmltidy and csslint have been disabled since flycheck does not
     ;; support more than one linter per major mode; specifically, all the
     ;; defined linters per major mode are executed regardless of the actual
     ;; file type.
     ;;      (flycheck-add-mode 'html-tidy 'web-mode)
     ;;      (flycheck-add-mode 'css-csslint 'web-mode)

     (flycheck-add-mode 'javascript-eslint 'web-mode)

     ;; IMPORTANT! Do not mess with the order in which the checkers are added
     ;; below.  Doing so will result in eslint being somehow overriden or
     ;; worse.
     (flycheck-add-mode 'javascript-flow 'web-mode)
     (flycheck-add-next-checker 'javascript-flow 'javascript-flow-coverage)
     (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)

     (add-hook 'flyspell-mode-hook 'init-flycheck/on-flyspell-mode)
     (add-hook 'flycheck-mode-hook 'init-flycheck/use-eslint-from-node-modules)))

;; Enable flycheck globally.
(global-flycheck-mode 1)


(defun init-flycheck/on-flyspell-mode()
  ;; Deactivate annoying correction of previous misspelled error when C-; is hit.
  (define-key flyspell-mode-map (kbd "C-;") nil))

;; Taken from:
;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;;
;; ... which was taken originally from:
;; https://emacs.stackexchange.com/q/21205/15089
(defun init-flycheck/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

;;; flycheck.el ends here
