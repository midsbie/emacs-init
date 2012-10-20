;; Set default window size
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
      (progn
        ;; use 180 char wide window for largeish displays
        ;; and smaller 90 column windows for smaller displays
        (if (> (x-display-pixel-width) 1280)
            (add-to-list 'default-frame-alist (cons 'width 190))
          (add-to-list 'default-frame-alist (cons 'width 90)))
        (add-to-list 'default-frame-alist 
                     (cons 'height 77)))))

;; Function: Set frame width
(defun set-frame-width-interactive (arg)
  (interactive "p")
  (set-frame-width (selected-frame) arg))

;; Function: Set frame height
(defun set-frame-height-interactive (arg)
  (interactive "p")
  (set-frame-height (selected-frame) arg))

(provide 'conf-frame-size)
