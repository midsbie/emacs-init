;;; early-init.el --- Settings applied before packages and the first frame  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools

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

;; Emacs loads this file before it activates packages, prepares the User Lisp
;; directory and creates the first frame.  ~/.emacs.d/early-init.el is a
;; symlink to it.

;;; Code:

;; Packages install on first launch into the gitignored elpa/ directory at the
;; root of the repository.  `file-truename' resolves the symlink this file is
;; loaded through.
(setq package-user-dir
      (expand-file-name "../elpa" (file-name-directory (file-truename load-file-name))))

;; Setup package archives
(setq package-archives
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
(setq package-quickstart t)

;; Avoid loading byte-compiled packages that are older than the source file.
(setq load-prefer-newer t)

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
