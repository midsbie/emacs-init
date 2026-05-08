;;; nxml.el --- Configures `nxml-mode'

;; Copyright (C) 2026  Miguel Guedes

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
  :mode (("\\.csproj\\'"  . nxml-mode)
         ("\\.fsproj\\'"  . nxml-mode)
         ("\\.vbproj\\'"  . nxml-mode)
         ("\\.vcxproj\\'" . nxml-mode)
         ("\\.props\\'"   . nxml-mode)
         ("\\.targets\\'" . nxml-mode)
         ("\\.nuspec\\'"  . nxml-mode)))

;;; nxml.el ends here
