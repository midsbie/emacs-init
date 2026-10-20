;;; init-modeline.el --- Mode line display and root warnings  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(custom-theme-set-faces
 'user
 '(vc-up-to-date-state    ((t (:inherit shadow))))
 '(vc-edited-state        ((t (:inherit success))))
 '(vc-locally-added-state ((t (:inherit font-lock-constant-face))))
 '(vc-needs-update-state  ((t (:inherit warning))))
 '(vc-conflict-state      ((t (:inherit (bold error)))))
 '(vc-missing-state       ((t (:inherit error))))
 '(vc-removed-state       ((t (:inherit warning))))
 '(vc-locked-state        ((t (:inherit font-lock-constant-face))))
 '(vc-ignored-state       ((t (:inherit shadow)))))

(my/modeline-mode 1)
(add-hook 'after-change-major-mode-hook #'my/warn-when-root-visits-file)

(provide 'init-modeline)

;;; init-modeline.el ends here
