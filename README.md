# infix.el

## Infix operators for Emacs Lisp

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

## Doesn't require spaces

Elisp parses `1+2` as a symbol, strange though it is.
Normally you can actually use this like any other symbol, for example
assigning it a value with `(setq 1+2 0)`. The `$` macro automatically
breaks this out and re-parses it as `1 + 2` before translating it
to `(+ 1 2)`.

If you don't want this behavior (which can admittedly be confusing
in the presence of variables `named-like-this`), you can use the `$:`
macro instead, which does not do any mangling of symbols.

## Plays nicely with s-exprs

The `$` macro _only rearranges the top-level terms it is applied to_.
Nested s-expressions are left alone, so you can do stuff like:

        ($ 1 + (read "2"))

This makes using normal elisp code within an infix expression very simple.
However, it might be confusing if you want to use parentheses the way they'd
be used in infix expressions in other languages: to override the default
order of operations. For example, consider the expression `(1+2)*3`.
For that you have two options:
        
Either use curly braces for grouping nested infix expressions:

        ($ {1 + 2} * 3)
        
Or just use the `$` macro again in a nested s-expression:

        ($ ($ 1 + 2) * 3)

## Generates efficient elisp

There is no need to worry about expressions like:

        ($ a + b + c + x + y + z)

resulting in inefficient elisp such as:

        (+ (+ (+ (+ (+ a b) c) x) y) z)

infix.el knows that some operators, like `+`, can have nested calls
simplified and will generate the desired expression:

        (+ a b c x y z)
        
## Lets you define your own operators

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

