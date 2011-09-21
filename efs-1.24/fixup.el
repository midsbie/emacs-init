;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         fixup.el
;; Release:      $efs release: 1.24 $
;; Version:      $Revision: 1.3 $
;; RCS:          
;; Description:  Fix up the load path for batch byte compilation of efs.
;; Author:       Andy Norman, Dawn
;; Created:      Sat Jan 30 00:21:33 1993
;; Language:     Emacs-Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq load-path
      (append (list (substitute-in-file-name "$CWD")
		    (substitute-in-file-name "$BDIR")
		    (substitute-in-file-name "$DIREDDIR")
		    (substitute-in-file-name "$CUSTOMDIR")
		    )
	      load-path))

(setq byte-compile-warnings t)

;; If the V18 btye-compiler is being used, this won't be around, so don't
;; complain if we can't find it.
(load "bytecomp-runtime" t t)

(load "bytecomp" nil t)

;; It seems efs causes the standard byte compiler some grief here.
(setq max-lisp-eval-depth (* 2 max-lisp-eval-depth))

;; If the user has the standard dired loaded, having dired
;; featurep will cause efs-dired.el to attempt to do overloads.
(delq 'dired features)

;;; end of fixup.el
