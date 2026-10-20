;;; init-erc.el --- Configure erc  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package erc
  :ensure nil
  :custom
  (erc-modules '(autojoin button completion fill irccontrols list
                 match menu move-to-prompt netsplit
                 networks noncommands notifications
                 readonly ring stamp track)))

(provide 'init-erc)

;;; init-erc.el ends here
