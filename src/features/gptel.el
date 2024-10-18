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
  :config
  (when (and (boundp 'gptel--openai-models)
             (assoc 'gpt-4o-mini gptel--openai-models))
    (customize-set-variable 'gptel-model 'gpt-4o-mini)))

;;; gptel.el ends here
