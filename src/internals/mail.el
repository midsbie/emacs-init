;;; mail.el --- Initialisation of mail related features

;; Copyright (C) 2014-2016 Miguel Guedes

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

;;; Commentary:

;;; Code:

;; NNTP newsgroups
(eval-after-load 'gnus
  '(progn
     (setq gnus-select-method '(nntp "eunews.blocknews.net")
           gnus-select-method ;; gnus-secondary-select-methods
           '(nnimap "Personal"
                    (nnimap-address "imap.gmail.com")
                    (nnimap-server-port 993)
                    (nnimap-stream ssl)))
     ))

;; SMTP
(eval-after-load 'message
  '(setq message-send-mail-function 'smtpmail-send-it))

(eval-after-load 'smtpmail
  '(setq smtpmail-default-smtp-server "smtp.gmail.com"
         smtpmail-smtp-server "smtp.gmail.com"
         smtpmail-smtp-service 587
         smtpmail-local-domain "miguelguedes.org"))

;;; mail.el ends here