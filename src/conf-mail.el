
;; Setup NNTP newsgroups
(setq gnus-select-method '(nntp "eunews.blocknews.net")
      user-full-name "Miguel Guedes"
      user-mail-address "miguel.a.guedes@gmail.com"
      nntp-authinfo-file "~/.authinfo"
      gnus-read-active-file nil)

;; Setup 
(setq gnus-select-method ;; gnus-secondary-select-methods
      '(nnimap "Personal"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "miguel.a.guedes@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "abstratti.com")
