;;; my-common-theme.el --- Shared face preferences  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Share GUI typography and VC state faces across display themes without
;; registering saved user customizations.

;;; Code:

(deftheme my-common
  "Shared typography and semantic version-control state faces.")

(custom-theme-set-faces
 'my-common

 ;; Keep in step with the `font' frame parameter in early-init.el.
 '(default ((((type graphic))
             (:inherit nil :stipple nil :inverse-video nil :box nil
              :strike-through nil :overline nil :underline nil
              :slant normal :weight normal :height 85 :width normal
              :family "Hack"))))

 '(vc-up-to-date-state    ((t (:inherit shadow))))
 '(vc-edited-state        ((t (:inherit success))))
 '(vc-locally-added-state ((t (:inherit font-lock-constant-face))))
 '(vc-needs-update-state  ((t (:inherit warning))))
 '(vc-conflict-state      ((t (:inherit (bold error)))))
 '(vc-missing-state       ((t (:inherit error))))
 '(vc-removed-state       ((t (:inherit warning))))
 '(vc-locked-state        ((t (:inherit font-lock-constant-face))))
 '(vc-ignored-state       ((t (:inherit shadow)))))

(provide-theme 'my-common)

;;; my-common-theme.el ends here
