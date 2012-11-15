(defun read-lines (file) 
  "Return a list of lines of a file given by FILE." 
  (with-temp-buffer 
    (insert-file-contents file) 
    (split-string (buffer-string) "\n" t)))

(provide 'libcommon)
