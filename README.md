# A-linear-lisp

A minimal linear lisp experiment

A-linear-lisp (ALL) is an expression lisp in administrative normal form,
all variables are linear and must be used exactly once.

## Forms:

let-binding:

    (let (id value)
        expression)

if branching:

    (if id
        then-expression
        else-expression)

function declaration:

    (fn (id id1 ... idn)
        expression)

function call:

    (id id1 ... idn)

drop - discard a linear value and then continue evaluation of expression:

    (drop id expression)

split - linearly break apart a cons cell (id3) into two parts (id1 and id2) and then continue
evaluation of expression:

    (split (id1 id id3)
        expression)

## Primitive operations

    (== id1 id2)
    (< id1 id2)
    (> id1 id2)
    (+ id1 id2 ...)
    (concat id1 id2 ...)
    (cons id1 id2)
    (clone id)

## Values

Strings:

    "Hello world"

Integers:

    1
    124
    256

Booleans:

    #t
    #f

## Example

    (fn (min a b)
        -- consume `a` making 2 copies which are stored in the cons cell `aa`
        (let (aa (clone a))
            (let (bb (clone b))
                -- consume cons cell `aa`, placing the car into `a1` and the cdr into `a2`.
                -- both `a1` and `a2` contain copies of the value from `a`
                (split (a1 a2 aa)
                    (split (b1 b2 bb)
                        -- our comparison consumes both `a1` and `a2`
                        (let (cmp (> a1 b1))
                            (if cmp
                                -- if cmp is true, then drop `a2` and evaluate to `b2`
                                (drop a2 b2)
                                -- if cmp is false, drop `b2` and evaluate to `a2`
                                (drop b2 a2))))))))

    (let (q 10)
         (let (z 20)
            (min q z))) -- evaluated to 10


It is important to note that `clone`, `split`, and `drop` are a bit "odd".

This comes down to our interpretation of a strict linear rule;
every value must be used once and exactly once in every branch of execution.
It is a compile-time error to use a variable more than once in the same branch,
likewise it is a compiler time error to not use a variable within a branch.

This is why our `min` function must first `clone` the arguments so that we can
consume `a1` and `b1` in our comparison,
finally we then consume using `drop` the value we do not want,
and evaluate to the value we want to return.


