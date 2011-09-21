;;; cust-stub.el --- defcustom stub for emacs versions without custom.el

;; Copyright (C) 1997 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: extensions
;; Created: 1997-05-28

;; $Id: cust-stub.el,v 1.1 1998/08/22 17:03:55 sperber Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Emacs 19.34 and earlier, and XEmacs 19.14 and earlier, do not have the
;; `defcustom' or `defgroup' forms used for user option customization.
;; As of May 1997, many of my own programs use these forms, so I am
;; including a stub to provide backward-compatibility in earlier versions
;; of emacs.

;;; Code:

(or (featurep 'custom)
    (load "custom" t))

;;

(defmacro cust-stub-defgroup (&rest args) nil)

(defmacro cust-stub-defcustom (var value doc &rest args)
  (list 'defvar var value doc))

;;

(or (fboundp 'defgroup)
    (fset 'defgroup 'cust-stub-defgroup))

(or (fboundp 'defcustom)
    (fset 'defcustom 'cust-stub-defcustom))

;;

(provide 'cust-stub)

;;; cust-stub.el ends here
