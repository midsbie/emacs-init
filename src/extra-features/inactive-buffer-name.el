;;; inactive-buffer-name.el --- Display buffer name in inactive windows  -*- lexical-binding: t; -*-

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

;; Shows a discrete buffer name label in the top-right corner of inactive
;; windows using overlays.  When the first visible line is short enough, the
;; label floats in the available space.  When the line is long, the label
;; visually replaces the rightmost characters (the actual buffer content is
;; unchanged).  The overlay is window-specific so the same buffer displayed in
;; two windows is handled correctly.

;;; Code:

(defvar my/inactive-buffer-name-overlays nil
  "List of overlays showing buffer names in inactive windows.")

(defun my/update-inactive-buffer-names (&rest _)
  "Update buffer name overlays in inactive windows."
  (mapc #'delete-overlay my/inactive-buffer-name-overlays)
  (setq my/inactive-buffer-name-overlays nil)
  (let ((active (selected-window)))
    (walk-windows
     (lambda (win)
       (unless (or (eq win active)
                   (frame-parent (window-frame win)))
         (with-current-buffer (window-buffer win)
           (let* ((name (buffer-name))
                  (label (concat " " name " "))
                  (label-len (string-width label))
                  (win-width (- (window-body-width win)
                                (line-number-display-width)))
                  (target-col (- win-width label-len))
                  (start (window-start win))
                  (eol (save-excursion (goto-char start) (line-end-position)))
                  (line-col (save-excursion (goto-char eol) (current-column))))
             (if (< line-col target-col)
                 ;; Short line: float label at right edge
                 (let ((ov (make-overlay eol eol nil t)))
                   (overlay-put ov 'window win)
                   (overlay-put ov 'after-string
                                (concat (propertize " " 'display
                                                    `(space :align-to (- right-fringe ,label-len)))
                                        (propertize label 'face 'header-line)))
                   (push ov my/inactive-buffer-name-overlays))
               ;; Long line: stamp label over content at right edge
               (let* ((ov-start (save-excursion
                                  (goto-char start)
                                  (move-to-column target-col)
                                  (point)))
                      (ov (make-overlay ov-start eol)))
                 (overlay-put ov 'window win)
                 (overlay-put ov 'display (propertize label 'face 'header-line))
                 (push ov my/inactive-buffer-name-overlays)))))))
     nil t)))

(add-hook 'window-selection-change-functions #'my/update-inactive-buffer-names)

;;; inactive-buffer-name.el ends here
