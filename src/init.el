;;; init.el --- Master emacs initialisation file

;; Copyright (C) 2014-2015 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; URL:
;; Keywords: tools
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

(defgroup init nil
  "Emacs initialisation mechanism."
  :group 'init)

(defvar init/path-base nil
  "Absolute path to Emacs' init `src´ directory.
If nil, `default-directory' is used instead.")

(defvar init/dirs-load '("internals" "features" "modes" "extensions")
  "Directories to automatically load.
Contains directories to automatically load as part of the
initialisation process.  Directories must be relative to
`init/path-base'.")

(defvar init/dir-packages "/usr/share/emacs/site-lisp/elpa"
  "Directory to packages.
Absolute path to directory containing packages managed by the
`package' feature.")

(defvar init/open-at-startup nil
  "List containing files to visit when Emacs finishes loading.

Files are only visited if the server hasn't yet been started.")

(defvar init/suppress-jshint t
  "When non-nil, causes the jshint linter to be ignored.")


(message "[init] starting")
(add-hook 'after-init-hook 'init/post-init)

;; set up include paths
(add-to-list 'load-path "/usr/src")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/packages")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/elpa")

;; load our common libraries and the `package' feature
(load-library "init/lib/libcommon")
(load-library "init/lib/libinit")
(require 'package)

;; setup and load ELPA packages (and others) first and foremost
(setq package-user-dir init/dir-packages)

;; add additional archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

;; This measure is needed to prevent tramp from hanging at startup as it tries
;; to conduct a strange check on the system's host.
;;
;; More information here: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20015
(setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ConnectTimeout=1 -o ControlPersist=no")

;; Now safe to load all ELISP source files in directories specified in
;; `init/dirs-load'
(when load-file-name
  (dolist (loading init/dirs-load)
    (let ((dir-loading (or init/path-base
                           (concat (file-name-directory load-file-name)
                                   loading))))
      (message "Loading ELISP files in: %s" dir-loading)
      (load-directory dir-loading))))

;; Load configurations specific to the environment
(if window-system
    (cond ((eq window-system 'x)
           (load "init/environment/x")))
  (load "init/environment/term"))

;; ----- Setup delayed initialisations
;;
;; Disable 'buffer * still has clients' message shown when killing buffers
;; spawned by emacsclient.
;; NOTE: for some reason the call to remove-hook needs to take place a few
;; seconds after emacs has launched.
(run-at-time "3 sec" nil
             '(lambda ()
                (remove-hook 'kill-buffer-query-functions
                             'server-kill-buffer-query-function)))

;; Load files in `init/open-at-startup' list after a short delay so as
;; enable the user to mutate the `init/open-at-startup' list.
(run-with-idle-timer
 0.1 nil
 '(lambda ()
    (dolist (file init/open-at-startup)
      (if (not (file-exists-p file))
          (message "%s" (concat "error: file does not exist: " file))
        (find-file file)
        (with-current-buffer (current-buffer)
          (when (eq major-mode 'org-mode)
            (org-shifttab 2)))
        (other-window 1)))))

;; It seems newer Emacs versions automatically enable electric indentation
;; mode.  Seeing as we don't like that, it is disabled right here.  Should also
;; be backwards compatible.
(when (and (> emacs-major-version 24) (fboundp 'electric-indent-mode))
  (electric-indent-mode -1))

(defun init/post-init ()
  "Perform post-init steps."
  (load "cl-lib")

  ;; Show how long it took to initialise emacs after 3 idle seconds.
  (run-with-idle-timer 3 nil '(lambda ()
                                (message "init took %s" (emacs-init-time)))))

(message "[init] done.")

;;; init.el ends here