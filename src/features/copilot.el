;;; copilot.el --- Customises the copilot package

;; Copyright (C) 2024  Miguel Guedes

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

;; Documentation found at the repository:
;; https://github.com/lanceberge/copilot

;;; Code:

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))
  :config
  ;; Known language identifiers:
  ;; https://code.visualstudio.com/docs/languages/identifiers
  (let ((extra-modes '(("c" . "c")
                       ("csharp" . "csharp")
                       ("css" . "css")
                       ("fsharp" . "fsharp")
                       ("go" . "go")
                       ("scss" . "scss")
                       ("sql" . "sql")
                       ("typescript" . "typescript"))))
    (dolist (item extra-modes)
      (unless (assoc (car item) copilot-major-mode-alist)
        (setq copilot-major-mode-alist (append copilot-major-mode-alist (list item)))))))

;;; copilot.el ends here
