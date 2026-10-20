;;; init-windows.el --- Window display and layout configuration -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; General window placement, layout history and navigation bindings.

;;; Code:

(require 'cl-lib)

(defun init/windows/display-adjacent (buffer alist)
  "Display BUFFER in a horizontal neighbour for other-window requests.
Act only when ALIST has a non-nil `inhibit-same-window' entry.  Prefer
windows sharing the selected window's right edge, then its left edge.
Skip dedicated, side and `no-other-window' windows.  Return the window
used, or nil to let the remaining display actions handle the request."
  (when (cdr (assq 'inhibit-same-window alist))
    (when-let* ((window
                 (cl-loop for edge in '(right left)
                          thereis
                          (cl-find-if
                           (lambda (candidate)
                             (not (or (window-dedicated-p candidate)
                                      (window-parameter candidate 'window-side)
                                      (window-parameter candidate 'no-other-window))))
                           (windows-sharing-edge nil edge)))))
      (display-buffer-use-some-window
       buffer (cons (cons 'some-window (lambda (_buffer _alist) window))
                    alist)))))

(defun init/windows/other-window-prefix (function &rest arguments)
  "Prefer adjacent windows for FUNCTION's next-command display.
FUNCTION is the original `other-window-prefix'; ARGUMENTS are passed
through.  Retain its fallback and one-command cleanup."
  (let ((register (symbol-function 'display-buffer-override-next-command)))
    (cl-letf (((symbol-function 'display-buffer-override-next-command)
               (lambda (prepare &optional post echo)
                 (funcall register
                          (lambda (buffer alist)
                            (if-let* ((window
                                       (init/windows/display-adjacent
                                        buffer (cons '(inhibit-same-window . t)
                                                     alist))))
                                (cons window 'reuse)
                              (funcall prepare buffer alist)))
                          post echo))))
      (apply function arguments))))

;; Base actions follow buffer-specific rules and explicit display actions,
;; including requests for contextual panes below the selected window.
(let ((functions (car display-buffer-base-action)))
  (when (functionp functions)
    (setq functions (list functions)))
  (setopt display-buffer-base-action
          (cons (cons #'init/windows/display-adjacent
                      (remq #'init/windows/display-adjacent functions))
                (cdr display-buffer-base-action))))

;; The prefix installs an overriding action that bypasses base actions.
;; Wrapping its registration preserves Emacs's fallback and cleanup logic.
(advice-add 'other-window-prefix :around #'init/windows/other-window-prefix)

(setopt help-window-select 'other)

;; Following list of buffers shouldn't open a new window.
(setopt same-window-buffer-names '("*shell*"
                                  "*mail*"
                                  "*unsent mail*"
                                  "*info*"))

;; This should be improved upon to take into account the dimensions of the
;; display and make splitting behave accordingly.
(setopt split-width-threshold nil
        split-height-threshold nil)

;; Stop the *Warnings* buffer from popping up intrusively and stealing focus.
;; `display-warning' tags its display request with the `warning' category, so
;; this only suppresses the automatic pop-up; the buffer can still be shown
;; deliberately (e.g. `C-x b *Warnings*').
(add-to-list 'display-buffer-alist
             '((category . warning)
               (display-buffer-no-window)
               (allow-no-window . t)))

(winner-mode)                           ; window configuration mutation undo

(global-set-key [M-left]              #'windmove-left)
(global-set-key [M-right]             #'windmove-right)
(global-set-key [M-up]                #'windmove-up)
(global-set-key [M-down]              #'windmove-down)
(global-set-key (kbd "C-x p")         #'my/other-previous-window)
(global-set-key (kbd "C-x 4 k")       #'my/kill-other-buffer)
(global-set-key (kbd "C-x 4 /")       #'my/bury-other-buffer)

;; Next to the built-in window commands; `C-x +' stays `balance-windows'.
(define-key window-prefix-map (kbd "=") #'my/rearrange-desktop)
;; Press [pause] in each window you want to "freeze" (also on `C-x w d').
(global-set-key [pause]               #'toggle-window-dedicated)

;; Disable the two-column minor mode keymap entirely as it is easy to trigger
;; accidentally and disrupts the window layout.
(global-set-key [f2]                  nil)

(provide 'init-windows)

;;; init-windows.el ends here
