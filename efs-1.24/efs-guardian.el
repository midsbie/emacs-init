;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-guardian.el
;; Release:      $efs release: 1.24 $
;; Version:      $Revision: 1.10 $
;; Description:  Guardian support for efs
;; Author:       Sandy Rutherford <sandy@math.ubc.ca>
;;               and Mike Sperber <sperber@informatik.uni-tuebingen.de>
;; Created:      Sat Jul 10 12:26:12 1993 by sandy on ibm550
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warranty (there isn't one) information.

;;; Acknowledgements:
;;; Adrian Philips, David Karr and Richard Bielawski for answering
;;; questions and debugging. Thanks.

(defconst efs-guardian-version
  (concat (substring "$efs release: 1.24 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.9 $" 11 -2)))

(provide 'efs-guardian)
(require 'efs)

;;;; ------------------------------------------------------------
;;;; Support for Tandem's GUARDIAN operating system.
;;;; ------------------------------------------------------------

;;;  Supposed to work for (Version 2.7 TANDEM 01SEP92).

;;; Explanations by Richard Bielawski <Richard_Bielawski@mastercard.com>:

;; When a user ID is added to a Guardian system the administrator must
;; assign a default directory for that ID.  All directory names take
;; the form \SYSTEM.$VOLUME.SUBVOL (On older systems the \SYSTEM part
;; is optional).  Notice there are 3 parts to a directory name.  Each
;; part is seperated by a dot.
;; 
;; All filenames are of the form \SYSTEM.$VOLUME.SUBVOL.FILENAME.
;; Notice there are 4 parts to a file name.  The 3 parts that specify
;; the directory and the last part names the specific file.
;; 
;; When referring to a file, any portion of the name that you leave
;; out is resolved from your defaults.  Here are some examples where
;; the users default directory is \SYSTEM.$VOLUME.SUBVOL:
;; 
;; When you specify        Guardian knows you really mean
;; ------------------      -----------------------------
;; MYFILE                  \SYSTEM.$VOLUME.SUBVOL.MYFILE
;; YOURDIR.AFILE           \SYSTEM.$VOLUME.YOURDIR.AFILE
;; $DATA1.BFILE            \SYSTEM.$DATA1.SUBVOL.BFILE
;; $DATA2.TEST3.CFILE      \SYSTEM.$DATA2.TEST3.CFILE
;; \YOURSYS.MYFILE         \YOURSYS.$VOLUME.SUBVOL.MYFILE
;; 
;; Note that in all cases the only part that is required is the
;; FILENAME part.  Guardian knows which parts you left out because the
;; system part starts with \, the volume starts with $ & the subvol
;; part starts with [A-Z].
;; 
;; Each of the 4 parts can be a maximum of 8 characters (including the
;; required \ and $ for the system and volume parts).  The first
;; character (after the \ or $ for system and volume parts) must be
;; [A-Z].  The remaining characters can be [A-Z0-9].
;; 
;; This matches a fully qualified file name.
;; (concat
;;   "^\\\\[A-Z][A-Z0-9]\\{0,6\\}\\."   ;system dot
;;   "\\$[A-Z][A-Z0-9]\\{0,6\\}\\."     ;volume dot
;;   "[A-Z][A-Z0-9]\\{0,7\\}\\."        ;subvol dot
;;   "[A-Z][A-Z0-9]\\{0,7\\}$")         ;filename
;; 
;; This matches a fully qualified directory name
;; (concat
;;   "^\\\\[A-Z][A-Z0-9]\\{0,6\\}\\."   ;system dot
;;   "\\$[A-Z][A-Z0-9]\\{0,6\\}\\."     ;volume dot
;;   "[A-Z][A-Z0-9]\\{0,7\\}$")         ;subvol


;;;  File name syntax:
;;;
;;;  File names are of the form volume.subvolume.file where
;;;  volume is $[alphanumeric characters]{1 to 7}
;;;  subvolume is <alpha character>[<alphanumeric character>]{0 to 7}
;;;  and file is the same as subvolume.

(defconst efs-guardian-date-regexp
  (concat
   " [ 1-3][0-9]-\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|"
   "Sep\\|Oct\\|Nov\\|Dec\\)-[0-9][0-9] "))

;;; entry points -- 2 of 'em.

(defun efs-reassemble-guardian-path (path reverse &optional one-component-p)
  (efs-save-match-data
    (let* ((case-fold-search t)
	   (path (if reverse
		     (efs-guardian-strip-system path)
		   path)))
      (let ((separator (if reverse "/" "."))
	    (r-systems (efs-guardian-path-systems-in-reverse path))
	    (r-volumes (efs-guardian-path-volumes-in-reverse path))
	    (r-components (efs-guardian-path-components-in-reverse path)))
	(let ((n-components (length r-components)))
	  (concat (if (null r-systems)
		      ""
		    (car r-systems))
		  (if (and (not (null r-systems))
			   (or (not (null r-volumes)) (not (null r-components))))
		      separator
		    "")
		  (if (null r-volumes)
		      ""
		    (car r-volumes))
		  (if (and (or (not (null r-systems))
			       (not (null r-volumes)))
			   (not (null r-components)))
		      separator
		    "")
		  (cond
		   ((= 0 n-components)
		    "")
		   ((or one-component-p
			(evenp n-components)) ; all directories
		    (car r-components))
		   (t ; directories + one filename
		    (concat
		     (cadr r-components)
		     separator
		     (car r-components))))))))))

(defun efs-guardian-strip-system (path)
  (if (string-match "^\\\\[A-Z][A-Z0-9]\\{0,6\\}\\."
		    path)
      (substring path (match-end 0) (length path))
    path))
	
(defun efs-guardian-path-components-in-reverse (path)
  ;; Extract all non-volume non-system components
  (let ((components '())
	(index 0))
    (while (string-match "\\([\./]\\|^\\)\\([A-Z][A-Z0-9]\\{0,7\\}\\)" path index)
      (setq components (cons (match-string 2 path) components))
      (setq index (match-end 2)))
    components))

(defun efs-guardian-path-volumes-in-reverse (path)
  ;; Extract all volumes
  (let ((volumes '())
	(index 0))
    (while (string-match "\\$[A-Z][A-Z0-9]\\{0,6\\}" path index)
      (setq volumes (cons (match-string 0 path) volumes))
      (setq index (match-end 0)))
    volumes))

(defun efs-guardian-path-systems-in-reverse (path)
  ;; Extract all system components
  (let ((systems '())
	(index 0))
    (while (string-match "\\\\[A-Z][A-Z0-9]\\{0,6\\}" path index)
      (setq systems (cons (match-string 0 path) systems))
      (setq index (match-end 0)))
    systems))

(efs-defun efs-fix-path guardian (path &optional reverse)
  ;; Convert PATH from unix-ish to guardian.
  ;; If REVERSE is non-nil do just that.
  (efs-reassemble-guardian-path path reverse))

(efs-defun efs-fix-dir-path guardian (dir-path)
  ;; Convert DIR-PATH from unix-ish to guardian for a DIR listing.
  (efs-save-match-data
    (let ((case-fold-search t))
      (if (string-equal dir-path "")
	  "*"
	(efs-reassemble-guardian-path dir-path nil t)))))

(efs-defun efs-parse-listing guardian
  (host user dir path &optional switches)
  ;; Parses a GUARDIAN DIRectory listing.
  ;; HOST = remote host name
  ;; USER = remote user name
  ;; DIR = remote directory as a remote full path
  ;; PATH = directory as an efs full path
  ;; SWITCHES are never used here, but they
  ;; must be specified in the argument list for compatibility
  ;; with the unix version of this function.
  (efs-save-match-data
    (goto-char (point-min))
    (if (re-search-forward efs-guardian-date-regexp nil t)
	(let ((tbl (efs-make-hashtable))
	      file size)
	  (while
	      (progn
		(beginning-of-line)
		(setq file (buffer-substring (point)
					     (progn
					       (skip-chars-forward "A-Z0-9")
					       (point))))
		(skip-chars-forward " ")
		(skip-chars-forward "^ ")
		(skip-chars-forward " ")
		(setq size (string-to-int (buffer-substring
					   (point)
					   (progn
					     (skip-chars-forward "0-9")))))
		(efs-put-hash-entry file (list nil size) tbl)
		(forward-line 1)
		(re-search-forward efs-guardian-date-regexp nil t)))
	  (efs-put-hash-entry "." '(t) tbl)
	  (efs-put-hash-entry ".." '(t) tbl)
	  tbl))))

(efs-defun efs-allow-child-lookup guardian (host user dir file)
  ;; Returns t if FILE in directory DIR could possibly be a subdir
  ;; according to its file-name syntax, and therefore a child listing should
  ;; be attempted.
  (efs-save-match-data
    (let ((case-fold-search t))
      (string-match "^/\\$[A-Z0-9]+/$" dir))))

(efs-defun efs-internal-file-directory-p guardian (file)
  ;; Directories pop into existence simply by putting files in them.
  (efs-save-match-data
    (let ((case-fold-search t))
      (if (string-match "^/\\$[A-Z0-9]+\\(/[A-Z0-9]+\\)?/?$" file)
	  t
	(efs-internal-file-directory-p nil file)))))

(efs-defun efs-internal-file-regular-p guardian (file)
  (efs-save-match-data
    (let ((case-fold-search t))
      (if (string-match "^/\\$[A-Z0-9]+\\(/[A-Z0-9]+\\)?/?$" file)
	  nil
	(efs-internal-file-regular-p nil file)))))

(efs-defun efs-internal-file-exists-p guardian (file)
  ;; Directories pop into existence simply by putting files in them.
  (efs-save-match-data
    (let ((case-fold-search t))
      (if (string-match "^/\\$[A-Z0-9]+\\(/[A-Z0-9]+\\)?/?$" file)
	  t
	(efs-internal-file-exists-p nil file)))))

;;; Tree Dired support

(defconst efs-dired-guardian-re-exe nil)

(or (assq 'guardian efs-dired-re-exe-alist)
    (setq efs-dired-re-exe-alist
	  (cons (cons 'guardian  efs-dired-guardian-re-exe)
		efs-dired-re-exe-alist)))

(defconst efs-dired-guardian-re-dir nil)

(or (assq 'guardian efs-dired-re-dir-alist)
    (setq efs-dired-re-dir-alist
	  (cons (cons 'guardian  efs-dired-guardian-re-dir)
		efs-dired-re-dir-alist)))

(efs-defun efs-dired-manual-move-to-filename guardian
  (&optional raise-error bol eol)
  ;; In dired, move to first char of filename on this line.
  ;; Returns position (point) or nil if no filename on this line.
  ;; This is the guardian version.
  (or eol (setq eol (save-excursion (skip-chars-forward "^\n\r") (point))))
  (if bol
      (goto-char bol)
    (skip-chars-backward "^\n\r")
    (setq bol (point)))
  (if (save-excursion (re-search-forward efs-guardian-date-regexp eol t))
      (progn
	(if (looking-at ". [^ ]")
	    (forward-char 2))
	(point))
    (and raise-error (error "No file on this line"))))

(efs-defun efs-dired-manual-move-to-end-of-filename guardian
  (&optional no-error bol eol)
  ;; Assumes point is at beginning of filename.
  ;; So, it should be called only after (dired-move-to-filename t).
  ;; On failure, signals an error or returns nil.
  ;; This is the guardian version.
  (and selective-display
       (null no-error)
       (eq (char-after
	    (1- (or bol (save-excursion
			  (skip-chars-backward "^\r\n")
			  (point)))))
	   ?\r)
       ;; File is hidden or omitted.
       (cond
	((dired-subdir-hidden-p (dired-current-directory))
	 (error
	  (substitute-command-keys
	   "File line is hidden. Type \\[dired-hide-subdir] to unhide.")))
	((error
	  (substitute-command-keys
	   "File line is omitted. Type \\[dired-omit-toggle] to un-omit.")))))
  (if (and
       (>= (char-after (point)) ?A)
       (<= (char-after (point)) ?Z)
       (progn
	 (skip-chars-forward "A-Z0-9")
	 (= (char-after (point)) ?\ )))
      (point)
    (and (null no-error)
	 (error "No file on this line"))))

(efs-defun efs-dired-ls-trim guardian ()
  (goto-char (point-min))
  (let (case-fold-search)
    (if (re-search-forward efs-guardian-date-regexp nil t)
	(progn
	  (beginning-of-line)
	  (delete-region (point-min) (point))
	  (forward-line 1)
	  (delete-region (point) (point-max))))))

;;; end of efs-guardian.el
