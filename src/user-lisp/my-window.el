;;; my-window.el --- Window utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: windows
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

;; Configuration loads after this file is compiled.
(defvar init/defaults/fill-column)

;; The following disabled as it was identified as the cause of strange
;; behaviour in Emacs 26 whereby opening a file would never occur in the active
;; window.
;; --
;; Note: I have no idea what the following does.
;; (defadvice pop-to-buffer (before cancel-other-window first)
;;   (ad-set-arg 1 nil))
;;
;; (ad-activate 'pop-to-buffer)

;;;###autoload
(defun my/other-previous-window (count &optional all-frames)
  "Select previous window in inverse cyclic ordering of windows.
COUNT specifies the number of windows to skip, starting with the
selected window, before making the selection.  If COUNT is
positive, skip COUNT backwards.  If COUNT is negative, skip COUNT
forwards.  In an interactive call, COUNT is the numeric prefix
argument.  If ALL-FRAMES is non-nill, window will be picked from
all currently active frames."
  (interactive "p")
  (other-window (* count -1) all-frames))

;;;###autoload
(defun my/rearrange-desktop ()
  "Rearrange the desktop.
Rearrange the desktop by creating as many windows as possible
that hold at least 85 characters.  Acts on the current frame
only."
  (interactive)
  (dotimes (_ (- (length (window-list)) 1))
    (delete-window (cadr (window-list))))
  (let* ((length (+ (or fill-column init/defaults/fill-column) 6))
         (count (- (/ (frame-width) length) 1))
         (buffers (buffer-list))
         (num-buffers (length buffers))
         (current-buffer 0))
    (dotimes (_ count)
      (split-window-right))
    (balance-windows)
    (dotimes (_ (1+ count))
        (while (and (< current-buffer num-buffers)
                    (not (buffer-file-name (nth current-buffer buffers))))
          (setq current-buffer (1+ current-buffer)))
        (when (< current-buffer num-buffers)
          (set-window-buffer (selected-window) (nth current-buffer buffers))
          (setq current-buffer (1+ current-buffer)))
        (other-window 1))))

(provide 'my-window)

;;; my-window.el ends here
