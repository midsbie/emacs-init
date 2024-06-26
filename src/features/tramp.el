;;; tramp.el --- Customises Tramp

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

(defun init/tramp/config ()
  "Configure the `tramp' package."

  ; Invoke by opening /sudo:sudo-zeus:/path/to/file
  (add-to-list 'tramp-default-proxies-alist
               '("sudo-zeus" nil "/ssh:softgeist.com:")))

;; Moved to after the function declaration so as to avoid potential failures at
;; load time
(use-package tramp
  :config (init/tramp/config))

;;; tramp.el ends here
