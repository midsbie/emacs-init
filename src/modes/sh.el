;;; sh.el --- Configures sh-mode

;; Copyright (C) 2015-2024  Miguel Guedes

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

(defun init/sh ()
  "Initialize sh-related modes."
  (setq-default  sh-basic-offset    2
                 sh-indentation     2))

(defun init/sh/enable ()
  "Initialise modes related to shell scripting development."
  (unless (init/sh/determine-script-mode)
    (init/common-nonweb-programming-mode)
    (auto-fill-mode -1)
    ;; Disable to prevent frequent freezes. Unfortunately, deinitialization has
    ;; to be deferred or it won't take.
    (run-with-idle-timer .5 nil #'(lambda() (company-mode -1)))))

(defun init/sh/determine-script-mode ()
  "Determine the appropriate mode for a script based on the shebang line."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "#!\\(.*\\)")
      (let ((interpreter (match-string 1)))
        (cond
         ((string-match "bash" interpreter)
          (unless (eq major-mode 'bash-ts-mode)
            (bash-ts-mode)))
         )))))

(use-package sh-script
  :hook ((sh-mode . init/sh/enable))
  :init (init/sh))

(use-package bash-ts-mode
  :hook ((bash-ts-mode . init/sh/enable))
  :init (init/sh))

;;; sh.el ends here