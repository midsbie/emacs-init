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
     ;; + in c/c++ modes
     (setq flycheck-cppcheck-checks        '("all")
           flycheck-c/c++-clang-executable "true") ; disable clang since we use
                                                   ; omnis

     ;; + in `php-mode'
     (setq flycheck-php-phpmd-executable "phpmd"
           flycheck-phpmd-rulesets '("cleancode" "codesize" "controversial"
                                     "design" "naming" "unusedcode"))

     ;; + in `web-mode'
     (flycheck-add-mode 'html-tidy 'web-mode)
     (flycheck-add-mode 'css-csslint 'web-mode)
     (flycheck-add-mode 'javascript-eslint 'web-mode)

     (add-hook 'flyspell-mode-hook 'init-flyspell-mode)))

(global-flycheck-mode 1)

(defun init-flyspell-mode()
  ;; Deactivate annoying correction of previous misspelled error, by default.
  (define-key flyspell-mode-map (kbd "C-;") nil))

;;; flycheck.el ends here
