infix.el
========

Infix operators for Emacs Lisp

This library provides a macro, `$`, which rewrites expressions written
in infix notation (with operators between their operands) to
S-expressions. This lets you write some Emacs Lisp code in a more
readable and concise format. For example:

        ($ x+y*3 < 15 and z%8 = 0)

expands to:

        (and (< (+ x (* y 3)) 15) (= (% z 8) 0))

Because the translation from infix notation happens during macro
expansion, it can be expected to have no performance penalty on
byte-compiled code, compared to writing the S-expression manually.

Additionally, there is no need to worry about expressions like:

        ($ a + b + c + x + y + z)

resulting in inefficient elisp such as:

        (+ (+ (+ (+ (+ a b) c) x) y) z)

infix.el knows that some operators, like `+`, can have nested calls
simplified and will generate the desired expression:

        (+ a b c x y z)

Finally, macros are also provided for declaring your own infix
operators with custom associativity and precedence.

For example, to declare the symbol `@` to be a left-associative
operator with the same precedence as the `+` operator, you can write:

        (infixl (precedence +) @)

To declare it as a right associative operator, with a precedence
higher than `+` but less than `*`, you would use:

        (infixr (precedence-between + *) @)

And finally, if `@` can have nested calls flattened to a single call,
as in the example of `+` above, you would use `infixrf` or `infixlf`
instead of `infixr` and `infixl`, respectively.

        (infixrf (precedence-between + *) @)
        ;; ($ 1 @ 2 @ 3) now becomes (@ 1 2 3) instead of (@ 1 (@ 2 3))

For further details, consult the docstrings for `$` and `$:`, or read
the comment blocks in the source.

Enjoy.

