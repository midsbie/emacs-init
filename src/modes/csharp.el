;;; csharp.el --- Configures `csharp-mode'

;; Copyright (C) 2021-2026  Miguel Guedes

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

;; C# buffers are served by csharp-ls through Eglot.  Navigation into compiled
;; assemblies (decompiled `csharp:/' sources) is handled in features/eglot.el.

;;; Log:

;; `format-all-mode' is now enabled conditionally when a .clang-format file
;; detected in the project.  See `init/format-all/csharp-maybe-enable' for
;; details.

;;; Code:

(defun init/csharp-mode/toggle-ts ()
  "Toggle between `csharp-mode' and `csharp-ts-mode'.
`csharp-ts-mode' is preferred but has indentation edge cases that
can be worked around by temporarily switching to `csharp-mode'."
  (interactive)
  (pcase major-mode
    ('csharp-mode (csharp-ts-mode))
    ('csharp-ts-mode (csharp-mode))
    (_ (user-error "Not in a C# buffer"))))

(defun init/csharp-mode/enable ()
  "Set up a buffer in `csharp-mode' or `csharp-ts-mode'."
  ;; Teach `M-q' to reflow `"..." + "..."' string concatenations (only in the
  ;; tree-sitter mode).  Chain to the mode's own filler for everything else so
  ;; comment filling keeps working.  See `extra-features/csharp-strings.el'.
  (when (treesit-parser-list nil 'c-sharp)
    (unless (eq fill-paragraph-function #'my/csharp-fill-paragraph)
      (setq-local my/csharp--prev-fill-paragraph-function fill-paragraph-function))
    (setq-local fill-paragraph-function #'my/csharp-fill-paragraph))

  (init/common-nonweb-programming-mode))

(use-package csharp-mode
  :mode ("\\.cs\\'" . csharp-ts-mode)
  :hook ((csharp-mode csharp-ts-mode) . init/csharp-mode/enable)
  :bind (:map csharp-mode-map
         ("C-c C-t" . init/csharp-mode/toggle-ts)
         :map csharp-ts-mode-map
         ("C-c C-t" . init/csharp-mode/toggle-ts)))

;;; csharp.el ends here
