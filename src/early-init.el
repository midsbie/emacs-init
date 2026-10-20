;;; early-init.el --- Settings applied before packages and the first frame  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Emacs loads this file before it activates packages, prepares the User Lisp
;; directory and creates the first frame.  ~/.emacs.d/early-init.el is a
;; symlink to it.

;;; Code:

;; Packages install on first launch into the gitignored elpa/ directory at the
;; root of the repository.  `file-truename' resolves the symlink this file is
;; loaded through.
(setopt package-user-dir
        (expand-file-name "../elpa" (file-name-directory (file-truename load-file-name))))

;; Setup package archives
(setopt package-archives
        '(("GNU ELPA" . "https://elpa.gnu.org/packages/")
          ("MELPA stable" . "https://stable.melpa.org/packages/")
          ("MELPA unstable" . "https://melpa.org/packages/")
          ("NON-GNU" . "https://elpa.nongnu.org/nongnu/"))
        package-archive-priorities
        '(
          ;; MELPA stable disabled as not all packages support stable channel
          ;; ("MELPA stable" . 10)
          ("MELPA unstable" . 9)
          ("GNU ELPA" . 5)))

;; Activate packages from one precomputed autoloads file, which package.el
;; regenerates whenever it installs or deletes a package.
(setopt package-quickstart t)

;; Avoid loading byte-compiled packages that are older than the source file.
(setopt load-prefer-newer t)

;; Turned off here so that the first frame is not drawn with them.  Builds
;; without a window system do not define the tool bar or scroll bar modes.
(menu-bar-mode -1)
(and (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(and (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Create graphical frames at their final size and font.  The first frame is
;; created after this file loads but before init.el runs, so setting these later
;; makes it visibly resize once init finishes.  The font must match the default
;; face set in environment/init-x.el.  Terminal frames ignore all three.
(push '(width . 120) default-frame-alist)
(push '(height . 70) default-frame-alist)
(push '(font . "Hack-8.5") default-frame-alist)

;;; early-init.el ends here
