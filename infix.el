;;; infix.el --- Infix operators for Emacs Lisp

;; Copyright (C) 2013

;; Author: R. Peele
;; Keywords: languages, lisp, extensions

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

;; This library provides a macro, $, which rewrites expressions
;; written in infix notation (with operators between their operands)
;; to S-expressions. This lets you write some Emacs Lisp code in a
;; more readable and concise format. For example:
;;
;; ($ x+y*3 < 15 and z%8 = 0)
;;
;; expands to:
;;
;; (and (< (+ x (* y 3)) 15) (= (% z 8) 0))
;;
;; Because the translation from infix notation happens during macro
;; expansion, it can be expected to have no performance penalty on
;; byte-compiled code, compared to writing the S-expression manually.
;;
;; Additionally, there is no need to worry about expressions like:
;;
;; ($ a + b + c + x + y + z)
;;
;; resulting in inefficient elisp such as:
;;
;; (+ (+ (+ (+ (+ a b) c) x) y) z)
;;
;; infix.el knows that some operators, like +, can have nested calls
;; simplified and will generate the desired expression:
;;
;; (+ a b c x y z)
;;
;; Finally, macros are also provided for declaring your own infix
;; operators with custom associativity and precedence.

;;; Code:

(defun infixp (object)
  "Return t if OBJECT is infix operator metadata."
  (and (vectorp object) (eq :infix (elt object 0))))

;; Operator information is stored in vectors and associated with
;; symbols via the symbol property `fixity'. An operator metadata
;; vector is arranged as follows:

;; [ :infix        ; Constant tag.
;;   SYMBOL        ; Function symbol this operator corresponds to.
;;   ASSOCIATIVITY ; :left, :right, or :none (currently equivalent to :right).
;;   PRECEDENCE    ; Precedence number; higher numbers bind more tightly.
;;   FLAT          ; If non-nil, can flatten nested calls to this operator.
;; ]

(defun infix-get (symbol)
  "Get infix operator metadata for SYMBOL.

Returns nil if SYMBOL is not an operator."
  (let ((fix (get symbol 'fixity)))
    (and (infixp fix) fix)))

(defun infix-put (symbol assoc prec &optional flat alias)
  "Define an infix operator for SYMBOL.

ASSOC should be :left, :right, or :none. This will determine how
expressions are grouped between operators of the same precedence.
For example:

x op y op z

with ASSOC :left becomes:
(op (op x y) z)

with ASSOC :right becomes:
(op x (op y z))

ASSOC :none means you don't care which one is used.

PREC should be a number. Higher values bind more tightly; for
example, * has a higher precedence number than +.

If FLAT is non-nil, nested calls to the operator will be
rewritten to single calls. For example, (@ 1 + 2 + 3) expands
to (+ 1 2 3), not (+ (+ 1 2) 3).

If ALIAS is non-nil, then it will be the symbol that is treated
as an operator, but it will expand to SYMBOL when converted to an
S-expression.

If SYMBOL is a list, then ALIAS will be ignored and each element
of the list will be defined as an operator alias for the first
element of the list."
  (unless alias (setq alias symbol))
  (if (listp symbol)
      (mapc
       #'(lambda (s) (infix-put (car symbol) assoc prec flat s))
       symbol)
    (put (or alias symbol)
         'fixity
         (vector :infix symbol assoc prec flat))))

(defun infix-symbol (op) (elt op 1))
(defun infix-associativity (op) (elt op 2))
(defun infix-precedence (op) (elt op 3))
(defun infix-flat (op) (elt op 4))

;; The core of the infix expression processing is Dijkstra's shunting
;; yard algorithm, without support for grouping parens since those are
;; handled elsewhere.

;; This is used to convert to prefix notation, which is then reversed
;; to get postfix notation. The postfix notation is then processed
;; into an S-expression tree.

(defun infix-inferior (a b)
  "Return t if operator B should bind before operator A."
  (let ((pa (infix-precedence a))
        (pb (infix-precedence b)))
    (or (> pb pa)
        (and (= pb pa) (eq :left (infix-associativity a))))))

(defun infix-to-postfix (terms)
  "Translate a list of infix expression TERMS to postfix form.

Operators in the infix expression will be replaced with their
metadata from `infix-get' in the postfix expression."
  (let (stack output)
    (while terms
      (let* ((term (car terms))
             (op (and (symbolp term) (infix-get term))))
        (if (not op)
            (push term output)
          (while (and stack (infix-inferior op (car stack)))
            (push (pop stack) output))
          (push op stack))
        (setq terms (cdr terms))))
    (while stack
      (push (pop stack) output))
    (nreverse output)))

;; Need mapcan, but it'd be a waste to require 'cl for that.
(defun infix-mapcan (function list)
  (apply 'nconc (mapcar function list)))

(defun infix-flatten-arg (symbol arg)
  ;; Check ARG to see whether it is a cons starting with SYMBOL, and
  ;; if it is, yield its arguments (raising them up to the flattened
  ;; level), otherwise yield itself.
  (if (and arg (listp arg) (eq symbol (car arg)))
      (cdr arg)
    (list arg)))

(defun infix-flatten-expr (expr)
  ;; Assuming EXPR is a list representing a lisp expression, such as
  ;; (+ 1 2), flattens it by bringing the arguments of any inner
  ;; expressions with the same function symbol as the outer expression
  ;; up to the outer expression level.
  (when expr
    (let ((symbol (car expr)))
      (cons symbol
            (infix-mapcan
             (apply-partially #'infix-flatten-arg symbol)
             (cdr expr))))))

(defun infix-build-expr (op &rest args)
  "Build an S-expression from operator metadata OP and arguments ARGS."
  (let ((expr `(,(infix-symbol op) ,@args)))
    (if (infix-flat op)
        (infix-flatten-expr expr)
      expr)))

(eval-when-compile
  (defmacro infix-pop (st op)
    `(if ,st
         (pop ,st)
       (error "Missing argument to infix operator %S" (infix-symbol ,op)))))

(defun infix-process-postfix (terms)
  "Convert postfix from `infix-to-postfix' to an S-expression."
  (let (stack current)
    (while terms
      (let ((term (car terms)))
        (setq current
              (if (not (infixp term))
                  term
                (let ((snd (infix-pop stack term))
                      (fst (infix-pop stack term)))
                  (infix-build-expr term fst snd))))
        (push current stack))
      (setq terms (cdr terms)))
    (when (cdr stack)
      (error "Leftover terms in infix expression"))
    current))

(defun infix-expand-1 (terms)
  "Expand a simple infix expression.

No curly braces supported."
  (infix-process-postfix (infix-to-postfix terms)))

;; infix should support grouping with curly braces like so:

;; ($ 3 * { 5 + 2 })

;; as opposed to having to write

;; ($ 3 * ($ 5 + 2))

;; To accomplish this, the expression is processed recursively with
;; the parts within braces being expanded to S-expressions
;; innermost-first. The code for this is mainly in `infix-subexpr*'.
;; Each call to this function returns a cons of the form:

;; (expression thus far . remaining terms)

;; In elisp I found that all the `let', `car', and `cdr'ing was really
;; cluttering the code and I was losing track of what I was doing. So
;; I wrote the algorithm I wanted in Haskell first:

;; ----------------------------------------------------------------------
;; data T a -- infix term
;;     = LB -- left brace
;;     | RB -- right brace
;;     | D a -- data
;;       deriving (Show, Read, Eq)
;; data E a -- expression tree
;;     = S a -- single data
;;     | L [E a] -- list of sub-expressions
;;       deriving (Show, Read, Eq)
;;
;; mcons :: E a -> E a -> E a
;; mcons x (L xs) = L (x : xs)
;; mcons x (S _) = undefined
;;
;; subexpr :: [T a] -> (E a, [T a])
;; subexpr [] = (L [], [])
;; subexpr (x : xs) =
;;     case x of
;;       D y ->
;;           (S y `mcons` f, r)
;;       RB -> (L [], xs)
;;       LB ->
;;           let (f2, r2) = subexpr r
;;           in (f `mcons` f2, r2)
;;     where
;;       (f, r) = subexpr xs
;; ----------------------------------------------------------------------

;; Then I translated that more or less directly to the elisp version
;; below. The algorithm used is the same, although the elisp version
;; calls `infix-expand-1' on the braced sub-expressions it finds.

(defun infix-subexpr* (terms)
  (if (not terms)
      '(nil . nil)
    (let ((x (car terms))
          (xs (cdr terms)))
      (if (eq x '})
          `(nil . ,xs)
        (let* ((s1 (infix-subexpr* xs))
               (f1 (car s1))
               (r1 (cdr s1)))
          (if (eq x '{)
              (let* ((s2 (infix-subexpr* r1))
                     (f2 (car s2))
                     (r2 (cdr s2)))
                `(,(cons (infix-expand-1 f1) f2) . ,r2))
            `(,(cons x f1) . ,r1)))))))

(defun infix-subexpr (terms)
  (let ((p (infix-subexpr* terms)))
    (if (cdr p)
        (error "Premature } in infix expression")
      (car p))))

;; Another feature is that spaces should be largely optional in infix
;; expressions. For example:

;; ($ 3*{5+2})

;; Ought to work the same as:

;; ($ 3 * { 5 + 2 })

;; Unfortunately for this purpose, elisp is very liberal about what
;; can constitute a symbol (pretty much anything that doesn't read as
;; another literal type), so it would treat all the terms stuck
;; together as one symbol and throw:

;; void-variable 3*{5+2}

;; I'll call this a glob symbol. To make this work, the terms of an
;; expression go through a deglobbing stage which splits glob
;; symbols into their constituent tokens, then re-reads those.

;; The rules for doing this are very simple. A given input symbol
;; produces an output token for each:

;; a. {
;; b. }
;; c. Consecutive series of alphanumeric characters, #, ., or _
;; d. Consecutive series of characters not in the above

(defun infix-split-symbol-name (name)
  "Split NAME, a string, into its constituent parts.

Returns a list of the tokens from NAME passed through the elisp reader."
  (when (and name (not (equal "" name)))
    (let* ((idx
            (string-match
             "^\\([}{]\\|[_.#[:alnum:]]+\\|[^_.#[:alnum:]}{]+\\)" name))
           (part (match-string-no-properties 1 name)))
      (cons (read part)
            (infix-split-symbol-name
             (substring name (+ (length part) idx)))))))

(defun infix-split-term (term)
  "Deglob TERM into a list of expression terms."
  (if (symbolp term)
      (infix-split-symbol-name (symbol-name term))
    (list term)))

(defun infix-split-terms (terms)
  "Return TERMS with glob symbols deglobbed."
  (infix-mapcan #'infix-split-term terms))

;; The deglobbing approach is pretty effective but does have some
;; warts. Any expression term with a mix of punctuation and
;; non-punctuation characters will get split, which may not always be
;; desirable. This is particularly salient in the case of the - sign,
;; since hyphenated variable names are very common in elisp.

;; I considered treating - as a word character like _, and simply
;; requiring spaces for subtraction. Instead I decided the safest
;; approach was to offer two infix macros, one with deglobbing for
;; concise math (where x-y means ``x minus y'', not ``the variable
;; named ex-why''), and another without deglobbing that works with any
;; variable and operator names at the expense of requiring whitespace
;; between each term.

(defmacro $ (&rest terms)
  "Rewrite the infix expression TERMS into an S-expression.

Terms in the infix expression are split according to
`infix-split-terms'. This means that spaces are not required
between most terms, however, complicated variable names with a
mix of punctuation and word characters may be misinterpreted.

See `$:' for a version of this macro that does not split
individual terms."
  (infix-expand-1 (infix-subexpr (infix-split-terms terms))))

(defmacro $: (&rest terms)
  "Rewrite the infix expression TERMS into an S-expression.

Spaces are required between each term (operator, operand, or
braces) of the infix expression."
  (infix-expand-1 (infix-subexpr terms)))

;; With the guts of the implementation done, the next step is to
;; define some macros for easy operator declaration.

(defmacro infixs (prec symbol assoc &optional flat)
  `(infix-put (quote ,symbol) ,assoc ,prec ,flat))
(defmacro infixl (prec symbol &optional flat)
  "Declare SYMBOL as a left-associative infix operator with precedence PREC."
  `(infixs ,prec ,symbol :left ,flat))
(defmacro infixlf (prec symbol)
  "Declare SYMBOL as a left-associative flat infix operator with precedence PREC."
  `(infixl ,prec ,symbol t))
(defmacro infixr (prec symbol &optional flat)
  "Declare SYMBOL as a right-associative infix operator with precedence PREC."
  `(infixs ,prec ,symbol :right ,flat))
(defmacro infixrf (prec symbol)
  "Declare SYMBOL as a right-associative flat infix operator with precedence PREC."
  `(infixr ,prec ,symbol t))
(defmacro infix (prec symbol &optional flat)
  "Declare SYMBOL as an infix operator with precedence PREC."
  `(infixs ,prec ,symbol :none ,flat))
(defmacro infixf (prec symbol)
  "Declare SYMBOL as a flat infix operator with precedence PREC."
  `(infix ,prec ,symbol t))

;; Finally, declare a standard set of infix operators.

;; I have left a gap of 256 between each level of precedence in the
;; standard operators. If you want to define a new operator with a
;; precedence between that of two existing operators, I recommend
;; using the exact halfway point. This leaves room for extension and
;; will work well with other custom operators defined using the same
;; approach.

(defun <| (function argument)
  "Apply FUNCTION to a single ARGUMENT."
  (apply function argument nil))

(infixlf #x0 (progn >>))

(infixr #x100 (setq <-))

(infixr #x200 <|)

(infixrf #x300 (or ||))

(infixrf #x400 (and &&))

(let ((p #x500))
  (infix p member)
  (infix p memq)
  (infix p equal)
  (infix p eq)
  (infix p =)
  (infix p /=)
  (infix p <)
  (infix p >)
  (infix p <=)
  (infix p >=))

(let ((p #x600))
  (infixr p cons)
  (infixr p append))

(let ((p #x700))
  (infixlf p +)
  (infixlf p -))

(let ((p #x800))
  (infixlf p *)
  (infixlf p /)
  (infixl p %)
  (infixl p mod))

(infixr #x900 ^)

(infixl #xa00 (elt !!))

(provide 'infix)

;; Considerations for improvement:

;; Support for prefix operators, particularly `null', would be very
;; useful.

;; It would be nice to rewrite `infix-expand-1' so that it converted
;; from infix to S-expr in one pass, without using an intermediate
;; postfix representation. However, this would be have to be simpler
;; than the current implementation; big elisp functions get confusing
;; fast.

;; Expression flattening, brace sub-expressions, symbol deglobbing --
;; all are currently written in recursive form. Emacs Lisp is not
;; great at recursion; fortunately I don't think infix code will be
;; written with hundreds of terms but it may be something to change
;; later anyway to be on the safe side.

;;; infix.el ends here
