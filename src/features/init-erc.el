;;; init-erc.el --- Configure erc -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configuration for the erc library.

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
