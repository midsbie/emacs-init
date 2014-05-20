;; window-extra.el --- Utility functions for manipulating windows and buffers
;;
;; Copyright (C) 2014 Miguel Guedes
;;
;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; URL: 
;; Keywords: windows
;; Version: 1.0
;;
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
;; Comments:
;; 
;;


(defadvice pop-to-buffer (before cancel-other-window first)
  (ad-set-arg 1 nil))

(ad-activate 'pop-to-buffer)

;; Toggle window dedication
(defun toggle-window-dedicated ()
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
(global-set-key [pause] 'toggle-window-dedicated)

(defun other-previous-window (count &optional all-frames)
  "Select previous window in inverse cyclic ordering of windows.

COUNT specifies the number of windows to skip, starting with the
selected window, before making the selection. If COUNT is positive,
skip COUNT backwards. If COUNT is negative, skip COUNT forwards. In
an interactive call, COUNT is the numeric prefix argument."
  (interactive "p")
  (other-window (* count -1) all-frames))

(defun kill-other-buffer (count &optional all-frames)
  "Select next window and kill buffer associated with it.

COUNT specified the number of windows to skip, starting with the
selected window, before making the selection. If COUNT is
positive, skip COUNT forwards. If COUNT is negative, skip COUNT
backwards. In an interactive call, COUNT is the numeric prefix
argument."
  (interactive "p")
  (save-selected-window
    (other-window (* count 1) all-frames)
    (kill-buffer)))

(defun bury-other-buffer (count &optional all-frames)
  "Select next window and bury buffer associated with it.

COUNT specified the number of windows to skip, starting with the
selected window, before making the selection. If COUNT is
positive, skip COUNT forwards. If COUNT is negative, skip COUNT
backwards. In an interactive call, COUNT is the numeric prefix
argument."
  (interactive "p")
  (save-selected-window
    (other-window (* count 1) all-frames)
    (bury-buffer)))

(provide 'window-extra)
