;;; init-modeline.el --- Modeline display and root warnings -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configure the custom modeline and highlight buffers opened as remote root.

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
