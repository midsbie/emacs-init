;;; init-git-commit.el --- Configure git-commit  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

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
Select the first yanked paragraph starting with \"irregularit\", ignoring case.
The fill is undone separately, so \\[undo] once restores the text as yanked."
  (interactive "*P")
  (yank arg)
  (undo-boundary)
  (my/git-commit/fill-paragraphs (min (point) (mark t)) (max (point) (mark t)))
  (let ((beg (min (point) (mark t)))
        (end (max (point) (mark t)))
        (case-fold-search t))
    (when-let* ((start (save-excursion
                        (goto-char beg)
                        (when (re-search-forward "^[ \t]*irregularit" end t)
                          (match-beginning 0)))))
      (goto-char start)
      (push-mark (min end (save-excursion (forward-paragraph) (point))) t t)
      (setq deactivate-mark nil))))

(use-package git-commit
  :ensure magit
  :init
  ;; Selection replacement runs before the command, so wrappers need this too.
  (put 'my/git-commit/yank-and-fill 'delete-selection 'yank)
  :bind (:map git-commit-mode-map
              ([remap yank] . my/git-commit/yank-and-fill)))

(provide 'init-git-commit)

;;; init-git-commit.el ends here
