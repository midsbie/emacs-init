;; libcommon --- Collection of useful defuns.
;;
;; Copyright (C) 2014 Miguel Guedes
;;
;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; URL:
;; Keywords: completion, convenience
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
;; Used by init.el.
;;


(defun read-lines (file) 
  "Return a list of lines of a file given by FILE." 
  (with-temp-buffer 
    (insert-file-contents file) 
    (split-string (buffer-string) "\n" t)))

;; Taken verbatim from: http://www.emacswiki.org/emacs/DotEmacsChallenge
(defun date (&optional insert)
  "Display the current date and time.
With a prefix arg, INSERT it into the buffer."
  (interactive "P")
  (funcall (if insert 'insert 'message)
           (format-time-string "%a, %d %b %Y %T %Z" (current-time))))

(defun warn-when-root-visits-file ()
  "Display the mode line with a red background when visiting a file with root
privileges."
  (when (string-match "^/su\\(do\\)?:" default-directory)
    (face-remap-add-relative
     'mode-line
     '(:background "red3" :foreground "white"))
    (face-remap-add-relative
     'mode-line-inactive
     '(:background "red4" :foreground "dark gray"
                   :box nil))
    )
  )


(provide 'libcommon)
