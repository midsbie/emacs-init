;;; git-commit.el --- Configures the git-commit feature

;; Copyright (C) 2026  Miguel Guedes

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar git-commit-need-summary-line)   ; Defined by git-commit.

(defun my/git-commit/summary-paragraph-p ()
  "Return non-nil if the paragraph at point holds the summary line."
  (and git-commit-need-summary-line
       (save-excursion
         (fill-forward-paragraph 1)
         (fill-forward-paragraph -1)
         (skip-chars-forward " \t\n")
         (= (line-beginning-position) 1))))

(defun my/git-commit/fill-paragraphs (beg end)
  "Fill each paragraph between BEG and END as \\[fill-paragraph] would.
The summary line is left alone, as `git-commit-setup-auto-fill' does."
  (save-excursion
    (let ((end (copy-marker end)))
      (goto-char beg)
      ;; Step into each paragraph first: from a blank line between paragraphs,
      ;; `log-edit-fill-entry' fills the previous one.
      (while (progn (skip-chars-forward " \t\n" end)
                    (< (point) end))
        (unless (my/git-commit/summary-paragraph-p)
          (fill-paragraph))
        (forward-paragraph))
      (set-marker end nil))))

(defun my/git-commit/yank-and-fill (&optional arg)
  "Yank as `yank' does with ARG, then fill each yanked paragraph.
The fill is undone separately, so \\[undo] once restores the text as yanked."
  (interactive "*P")
  (yank arg)
  (undo-boundary)
  (my/git-commit/fill-paragraphs (min (point) (mark t)) (max (point) (mark t))))

(use-package git-commit
  :bind (:map git-commit-mode-map
              ([remap yank] . my/git-commit/yank-and-fill)))

;;; git-commit.el ends here
