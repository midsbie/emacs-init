;;; dap.el --- Customises DAP

;; Copyright (C) 2022  Miguel Guedes

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

(defun init/dap-mode ()
  "Initialize `dap-mode'."
  ;; Disable the intrusive debugging toolbar positioned center-top of the
  ;; frame.  There doesn't seem to be an explicit configuration setting other
  ;; than the `dap-auto-configure-features' variable, but it seems specific to
  ;; the `dap-auto-configure-mode' command.  The following doesn't work:
  ;;
  ;;   (setq dap-auto-configure-features (remove 'controls dap-auto-configure-features))
  ;;
  ;; Using defun advice instead adapted from the following snippet:
  ;; https://github.com/emacs-lsp/dap-mode/issues/372#issuecomment-939248097
  (advice-add 'dap-debug :after #'init/dap/disable-dap-ui-controls-mode))

(defun init/dap/disable-dap-ui-controls-mode (orig-fun &rest args)
  (dap-ui-controls-mode -1))

(use-package dap-mode
  :disabled
  :init (init/dap-mode))

;;; dap.el ends here
