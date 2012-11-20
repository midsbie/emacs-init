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
COUNT specified the number of windows to skip, starting with the
selected window, before making the selection. If COUNT is positive,
skip COUNT backwards. If COUNT is negative, skip COUNT forwards. In
an interactive call, COUNT is the numeric prefix argument."
  (interactive "p")
  (other-window (* count -1)))

(provide 'window-extra)
