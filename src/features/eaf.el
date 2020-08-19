;;; eaf.el --- Customises the `eaf' package

;; Copyright (C) 2020  Miguel Guedes

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; From: https://github.com/MatthewZMD/.emacs.d#org2915d05
(use-package eaf
  :load-path (lambda () (expand-file-name "site-elisp/emacs-application-framework" user-emacs-directory))
  ;; `nil` below is effectively disabling the eaf package
  :if (and nil (display-graphic-p) python-p pip-p
           (not (equal (shell-command-to-string "pip freeze | grep '^PyQt\\|PyQtWebEngine'") "")))
  :bind (("C-c w g" . google)
         ("C-c w b" . browse-web))
  :custom
  (eaf-find-alternate-file-in-dired t)
  (browse-url-browser-function 'eaf-open-browser) ;; Make EAF Browser my default browser
  (eaf-browser-continue-where-left-off t)
  :config
  (defalias 'browse-web #'eaf-open-browser)
  (eaf-setq eaf-browser-default-zoom "1.25")
  (eaf-setq eaf-browser-dark-mode "false")
  (eaf-setq eaf-browser-enable-adblocker "true")
  (eaf-setq eaf-pdf-dark-mode "false")
  ;; I already bind "RET", "<mouse-2>", "^" to `dired-find-alternate-file' in `init-dired.el'.
  ;; Comment this line out of you don't want to use EAF to open available files in dired.
  ;; (global-set-key [remap dired-find-alternate-file] #'eaf-file-open-in-dired)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  (eaf-bind-key open_link "C-M-s" eaf-browser-keybinding)
  (eaf-bind-key clear_cookies "C-M-q" eaf-browser-keybinding)
  (eaf-bind-key insert_or_recover_prev_close_page "X" eaf-browser-keybinding)
  (eaf-bind-key scroll_up "RET" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "DEL" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "u" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_up_page "d" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_to_end "M->" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_to_home "M-<" eaf-pdf-viewer-keybinding)
  (eaf-bind-key quit-window "q" eaf-pdf-viewer-keybinding)
  (eaf-bind-key zoom_in "C-=" eaf-pdf-viewer-keybinding)
  (eaf-bind-key zoom_out "C--" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (eaf-bind-key eaf-send-key-sequence "M-]" eaf-terminal-keybinding)
  )

;;; eww.el ends here
