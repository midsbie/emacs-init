;;; window-extra.el --- Utility functions for manipulating windows

;; Copyright (C) 2014-2023 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; URL:
;; Keywords: windows
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:

;;; Code:

;; The following disabled as it was identified as the cause of strange
;; behaviour in Emacs 26 whereby opening a file would never occur in the active
;; window.
;; --
;; Note: I have no idea what the following does.
;; (defadvice pop-to-buffer (before cancel-other-window first)
;;   (ad-set-arg 1 nil))
;;
;; (ad-activate 'pop-to-buffer)

;; Toggle window dedication
(defun my/toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;; Press [pause] key in each window you want to "freeze"
(global-set-key [pause] 'my/toggle-window-dedicated)

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

(defun my/rearrange-desktop ()
  "Rearrange the desktop.
Rearrange the desktop by creating as many windows as possible
that hold at least 85 characters.  Acts on the current frame
only."
  (interactive)
  (dotimes (n (- (length (window-list)) 1))
    (delete-window (cadr (window-list))))
  (let* ((length (+ (or fill-column init/defaults/fill-column) 6))
         (count (- (/ (frame-width) length) 1))
         (buffers (buffer-list))
         (num-buffers (length buffers))
         (current-buffer 0))
    (dotimes (n count)
      (split-window-right))
    (balance-windows)
    (dotimes (n (1+ count))
        (while (and (< current-buffer num-buffers)
                    (not (buffer-file-name (nth current-buffer buffers))))
          (setq current-buffer (1+ current-buffer)))
        (when (< current-buffer num-buffers)
          (set-window-buffer (selected-window) (nth current-buffer buffers))
          (setq current-buffer (1+ current-buffer)))
        (other-window 1))))

;;; window-extra.el ends here
