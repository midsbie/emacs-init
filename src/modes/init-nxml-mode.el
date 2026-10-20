;;; init-nxml-mode.el --- Configure XML editing and MSBuild file associations  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Miguel Guedes

;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; Keywords: tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Routes the XML-format files used by MSBuild to `nxml-mode'.  Emacs ships with
;; no `auto-mode-alist' entries for these extensions despite the files being
;; well-formed XML, so without the mappings below nXML's tag matching, schema
;; validation, and structured editing do not kick in.
;;
;; Coverage is deliberately scoped to MSBuild's XML surface: project files
;; (`.csproj', `.fsproj', `.vbproj', `.vcxproj', `.proj'), property and target
;; sheets (`.props', `.targets'), and NuGet specs (`.nuspec').  The extension
;; patterns subsume well-known fixed-name files such as `Directory.Build.props'
;; and `Directory.Build.targets', so those do not need their own entries.

;;; Code:

(use-package nxml-mode
  :ensure nil
  :mode (("\\.csproj\\'"  . nxml-mode)
         ("\\.fsproj\\'"  . nxml-mode)
         ("\\.vbproj\\'"  . nxml-mode)
         ("\\.vcxproj\\'" . nxml-mode)
         ("\\.props\\'"   . nxml-mode)
         ("\\.targets\\'" . nxml-mode)
         ("\\.nuspec\\'"  . nxml-mode)))

(provide 'init-nxml-mode)

;;; init-nxml-mode.el ends here
