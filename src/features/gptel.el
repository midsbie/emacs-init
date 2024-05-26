;;; gptel.el --- Customises the gptel package

;; Copyright (C) 2023  Miguel Guedes

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

(defun init/gptel ()
  "Initialize the gptel package."
  (setq-default gptel-model "gpt-4o")

  ;; Provide a more convenient list of updated models.  Note that
  ;; "gpt-3.5-turbo-instruct" is not supported because it is not a _chat_ model
  ;; and is not available through the v1/completions endpoint.  Unfortunately
  ;; gptel uses v1/chat/completions, thus rendering the model unavailable.
  (setq gptel--openai
        (gptel-make-openai
         "ChatGPT"
         :header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
         :key 'gptel-api-key
         :stream t
         :models '("gpt-3.5-turbo-1106" "gpt-3.5-turbo" "gpt-3.5-turbo-16k"
                   "gpt-4-1106-preview" "gpt-4" "gpt-4-32k"))))

(use-package gptel
  :config
  (init/gptel))

;;; gptel.el ends here
