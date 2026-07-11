;;; csharp-strings.el --- Reflow C# string concatenations  -*- lexical-binding: t; -*-

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

;; `csharp-ts-mode' does not teach the fill machinery about C#'s idiom of
;; splitting a long string across lines with the `+' concatenation operator:
;;
;;     [Tooltip("How strongly aim is pulled to an acquired target. 0 keeps " +
;;              "the raw manual aim point.")]
;;
;; This file adds two tree-sitter based commands plus an `M-q' integration:
;;
;; - `my/csharp-string-join' collapses such a concatenation into a single
;;   string literal (the "deconstruct" step), ready for an external formatter.
;;
;; - `my/csharp-string-fill' reflows it to `fill-column', emitting `"..." +'
;;   continuation lines aligned under the opening quote -- i.e. formats it
;;   entirely within Emacs.
;;
;; - `my/csharp-fill-paragraph' is installed as the buffer-local
;;   `fill-paragraph-function' (see `init/csharp-mode/enable') so that plain
;;   `M-q' reflows a string when point is on one, and otherwise delegates to
;;   the mode's usual comment filler.
;;
;; No dedicated key is bound for the join: `M-Q' (`my/unfill-paragraph')
;; already calls `fill-paragraph' with an unbounded `fill-column', which routes
;; through the same `fill-paragraph-function' and collapses the concatenation
;; to a single line -- mirroring the global `M-q' / `M-Q' fill / unfill pair.
;; `my/csharp-string-join' remains available via \\[execute-extended-command].

;;; Code:

(require 'treesit)

(defun my/csharp--string-pieces (node)
  "Return the ordered list of `string_literal' NODEs forming a plain
C# string (concatenation) rooted at NODE, or nil if NODE is not a
pure concatenation of plain double-quoted string literals.

Verbatim (@\"...\"), interpolated ($\"...\") and raw strings are
intentionally rejected so they are never mangled."
  (cond
   ((null node) nil)
   ((equal (treesit-node-type node) "string_literal")
    (list node))
   ((and (equal (treesit-node-type node) "binary_expression")
         (equal (treesit-node-text
                 (treesit-node-child-by-field-name node "operator") t)
                "+"))
    (let ((l (my/csharp--string-pieces
              (treesit-node-child-by-field-name node "left")))
          (r (my/csharp--string-pieces
              (treesit-node-child-by-field-name node "right"))))
      (and l r (append l r))))
   (t nil)))

(defun my/csharp--string-target (pos)
  "Return (NODE . PIECES) for the outermost plain C# string
concatenation covering POS, or nil when POS is not on one."
  (condition-case nil
      (let ((node (treesit-parent-until
                   (treesit-node-at pos)
                   (lambda (n) (member (treesit-node-type n)
                                       '("string_literal" "binary_expression")))
                   t)))
        ;; Climb to the outermost binary_expression that is still a pure
        ;; string concatenation (stops at, e.g., `"a" + "b" + var').
        (while (let ((p (and node (treesit-node-parent node))))
                 (and p
                      (equal (treesit-node-type p) "binary_expression")
                      (my/csharp--string-pieces p)
                      (setq node p))))
        (when node
          (let ((pieces (my/csharp--string-pieces node)))
            (when pieces (cons node pieces)))))
    (error nil)))

(defun my/csharp--literal-content (node)
  "Return the raw source text between the quotes of string_literal NODE.
Everything inside the quotes is kept verbatim, including escape
sequences such as \\\" which the grammar exposes as separate
children."
  (let* ((s (treesit-node-text node t))
         (n (length s)))
    (if (and (>= n 2)
             (eq (aref s 0) ?\")
             (eq (aref s (1- n)) ?\"))
        (substring s 1 (1- n))
      s)))

(defun my/csharp--last-space (s from to)
  "Index of the last ?\\s in S within [FROM, TO), or nil."
  (let ((i (1- to)) (res nil))
    (while (and (>= i from) (not res))
      (when (eq (aref s i) ?\s) (setq res i))
      (setq i (1- i)))
    res))

(defun my/csharp--hex-digit-p (c)
  "Non-nil when character C is an ASCII hexadecimal digit."
  (or (and (>= c ?0) (<= c ?9))
      (and (>= c ?a) (<= c ?f))
      (and (>= c ?A) (<= c ?F))))

(defun my/csharp--atom-end (s pos len)
  "Return the end index of the indivisible atom starting at POS in S.
LEN is (length S).  An atom is a single ordinary character or a
complete C# escape sequence -- `\\\"', `\\n', `\\xH{1,4}', `\\uHHHH'
or `\\UHHHHHHHH' -- so breaking between atoms never severs an escape."
  (if (or (/= (aref s pos) ?\\) (>= (1+ pos) len))
      (1+ pos)                            ; ordinary char (or trailing backslash)
    (pcase (aref s (1+ pos))
      (?x (let ((j (+ pos 2)) (k 0))      ; \x + 1-4 hex digits (variable length)
            (while (and (< j len) (< k 4) (my/csharp--hex-digit-p (aref s j)))
              (setq j (1+ j) k (1+ k)))
            j))
      (?u (min len (+ pos 6)))            ; \uHHHH
      (?U (min len (+ pos 10)))           ; \UHHHHHHHH
      (_  (+ pos 2)))))                   ; simple two-character escape

(defun my/csharp--safe-break (s from end)
  "Move END back to an atom boundary in S so no escape is split.
Walk atoms from FROM; never break inside a `\\uHHHH', `\\xHH' or any
other escape.  Always returns a value greater than FROM -- a lone atom
wider than the budget is kept whole (the line overflows) rather than
severed."
  (let ((len (length s)) (pos from) (prev from))
    (while (< pos end)
      (setq prev pos
            pos  (my/csharp--atom-end s pos len)))
    (cond ((= pos end) end)               ; END already on an atom boundary
          ((> prev from) prev)            ; break before the straddling atom
          (t pos))))                      ; single over-long atom: keep it whole

(defun my/csharp--wrap-string (s indent width)
  "Split source string S into chunks that each render within WIDTH,
assuming the opening quote sits at column INDENT.
Every line -- including the last -- reserves the `\" +' overhead, which
also leaves two columns of headroom for the trailing delimiter (`)]',
`);', `,') that usually follows the string on the closing line."
  (let* ((overhead 4)                       ; opening `\"' + closing `\" +'
         (budget (max 1 (- width indent overhead)))
         (len (length s)))
    (if (<= (+ indent 2 len) width)         ; fits on one line (final overhead 2)
        (list s)
      (let ((chunks '()) (i 0))
        (while (< i len)
          (let* ((remaining (- len i))
                 (end (+ i (min budget remaining))))
            (when (< end len)
              (let ((sp (my/csharp--last-space s i end)))
                (when (and sp (> sp i)) (setq end (1+ sp))))
              (setq end (my/csharp--safe-break s i end))
              (when (<= end i)              ; guarantee forward progress
                (setq end (min len (+ i (min budget remaining))))))
            (push (substring s i end) chunks)
            (setq i end)))
        (nreverse chunks)))))

(defun my/csharp--render-chunks (chunks indent)
  "Render CHUNKS as `\"...\" +' lines aligned under column INDENT.
The first line carries no leading padding (the buffer prefix is
already there)."
  (let ((pad (make-string indent ?\s))
        (n (length chunks))
        (i 0)
        (out ""))
    (dolist (chunk chunks out)
      (setq out (concat out
                        (if (= i 0) "" pad)
                        "\"" chunk "\""
                        (if (< i (1- n)) " +\n" "")))
      (setq i (1+ i)))))

(defun my/csharp--last-atom-start (s)
  "Return the start index of the final atom in S, or nil when S is empty."
  (let ((len (length s)) (pos 0) (last nil))
    (while (< pos len)
      (setq last pos
            pos  (my/csharp--atom-end s pos len)))
    last))

(defun my/csharp--ends-with-short-hex-p (s)
  "Non-nil when S ends with a `\\x' escape carrying fewer than four hex
digits, which could greedily absorb a following hex digit."
  (let ((last (my/csharp--last-atom-start s))
        (len  (length s)))
    (and last
         (< (1+ last) len)
         (eq (aref s last) ?\\)
         (eq (aref s (1+ last)) ?x)
         (< (- len last) 6))))              ; `\x' + fewer than four hex digits

(defun my/csharp--pad-trailing-hex (s)
  "Left-pad the trailing `\\x' escape of S to four hex digits
\(`\\x4' -> `\\x0004'); the value is unchanged but its width is now fixed."
  (let* ((last   (my/csharp--last-atom-start s))
         (digits (substring s (+ last 2))))
    (concat (substring s 0 (+ last 2))
            (make-string (- 4 (length digits)) ?0)
            digits)))

(defun my/csharp--concat-pieces (contents)
  "Concatenate literal CONTENTS into one string body.
C#'s `\\x' escape is greedy over one to four hex digits, so a short
`\\x' ending one piece would silently absorb a leading hex digit of the
next (`\"\\x41\" + \"23\"' is `\"A23\"', not `\"\\x4123\"').  Pad such an
escape to four digits before joining so it can no longer cross the seam."
  (let ((acc ""))
    (dolist (c contents acc)
      (when (and (> (length c) 0)
                 (my/csharp--hex-digit-p (aref c 0))
                 (my/csharp--ends-with-short-hex-p acc))
        (setq acc (my/csharp--pad-trailing-hex acc)))
      (setq acc (concat acc c)))))

(defun my/csharp--replace-target (single-line)
  "Rewrite the C# string concatenation at point.
When SINGLE-LINE is non-nil, collapse to one literal; otherwise
reflow to `fill-column'."
  (let ((target (my/csharp--string-target (point))))
    (unless target
      (user-error "Point is not on a plain C# string (concatenation)"))
    (let* ((node    (car target))
           (pieces  (cdr target))
           (start   (treesit-node-start node))
           (end     (treesit-node-end node))
           (content (my/csharp--concat-pieces
                     (mapcar #'my/csharp--literal-content pieces)))
           (indent  (save-excursion (goto-char start) (current-column)))
           (text    (if single-line
                        (concat "\"" content "\"")
                      (my/csharp--render-chunks
                       (my/csharp--wrap-string content indent fill-column)
                       indent))))
      (unless (string= text (buffer-substring-no-properties start end))
        (save-excursion
          (delete-region start end)
          (goto-char start)
          (insert text)))
      t)))

(defun my/csharp-string-join ()
  "Collapse the multi-line C# string concatenation at point into a
single string literal, ready for an external formatter."
  (interactive)
  (my/csharp--replace-target t))

(defun my/csharp-string-fill ()
  "Reflow the C# string (concatenation) at point to `fill-column',
splitting it into `\"...\" +' continuation lines aligned under the
opening quote."
  (interactive)
  (my/csharp--replace-target nil))

(defvar-local my/csharp--prev-fill-paragraph-function nil
  "The `fill-paragraph-function' in effect before ours was installed.
Delegated to by `my/csharp-fill-paragraph' when point is not on a
plain C# string, so `csharp-ts-mode' comment filling still works.")

(defun my/csharp-fill-paragraph (&optional justify _region)
  "Fill a C# string concatenation at point.
When point is not on a plain string, delegate to the previous
`fill-paragraph-function' (usually `c-ts-common--fill-paragraph')
so comment filling is preserved.  Intended to be installed as
`fill-paragraph-function' via `init/csharp-mode/enable'."
  (cond
   ((my/csharp--string-target (point))
    (my/csharp-string-fill)
    t)
   ((and my/csharp--prev-fill-paragraph-function
         (not (eq my/csharp--prev-fill-paragraph-function
                  #'my/csharp-fill-paragraph)))
    (funcall my/csharp--prev-fill-paragraph-function justify))
   (t nil)))

;;; csharp-strings.el ends here
