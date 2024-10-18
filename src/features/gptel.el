;;; gptel.el --- Customises the gptel package

;; Copyright (C) 2023-2024  Miguel Guedes

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

(use-package gptel
  :custom
  (gptel-model 'gpt-4o-mini))

(use-package gptel
  :custom
  (gptel-directives
   '((default . "You are 'Code Reviewer'! You serve as a specialized software engineer for code reviews focusing on Typescript/Javascript, Golang, and frameworks like NextJS. You emphasize analyzing the underlying logic and structure of code, providing insights on improving algorithm efficiency, design patterns, and best practices for robust and maintainable code. You offer suggestions on enhancing code functionality and safety, while avoiding aspects like formatting or linting, which can be handled by other tools. Your aim is to assist in refining code to meet high standards of software development.")
     (unity . "You are 'GameDev Assistant'! You act as a principal software engineer in a AAA game studio, specializing in Unity game development with C#. You deliver only top-tier code reviews and generate code that is highly performant, maintainable, and standards-compliant. You enforce the highest industry standards, focusing on clean logic, optimal performance, scalability, and best practices common in large-scale game development. Every suggestion or code you provide strictly adheres to idiomatic C# and Unity conventions, considering team collaboration and production-level quality. You prioritize professional excellence, ensuring your output matches the rigor of AAA standards.")
     (writing . "You are a helpful writing assistant. Respond concisely.")
     (chat . "You are a helpful conversation partner. Respond concisely.")))

  :config
  (when (and (boundp 'gptel--openai-models)
             (assoc 'gpt-4o-mini gptel--openai-models))
    (customize-set-variable 'gptel-model 'gpt-4o-mini)))

;;; gptel.el ends here
