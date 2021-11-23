# pipechain

Provides a quick and easy way to reduce nested function calls without creating
many superfluous temporary names. Naming things is hard, but so is reading
nested function calls!

## Related Libraries

This package provides a similar macro to
[threading](https://pkgs.racket-lang.org/package/threading), but with some
simplification and generalization.

Unlike `~>` from that package, this version allows for arbitrarily complex
function calls so that `(~> arg (func1 _ (func2 _)))` works as expected.

Additionally, `_` is a syntax parameter which outside of a `~>` call is just the
[_](https://docs.racket-lang.org/reference/stx-patterns.html?q=_#(form._((lib._racket/private/stxcase-scheme..rkt).__)))
already built into racket. This is to try and ensure maximum compatibility with
other libraries that also use the underscore.

## Examples

### Full Example

```rkt
(~> arg
    func1
    func2
    (arg-func1 _ 42)
    (arg-func2 #:temp #t 11 _ "tree"))
```

is equivalent to this (the actual implementation avoids introducing unnecessary
scopes, `let*` is used here for readability):

```rkt
(let* ([_ (func1 arg)]
       [_ (func2 _)]
       [_ (arg-func1 _ 42)]
       [_ (arg-func2 #:temp #t 11 _ "tree")])
  _)
```

### Single Argument

```rkt
(~> arg)
```

is equivalent to:

```rkt
arg
```

### Simple Function Chaining

This rule applies since `func1` and `func2` are identifiers and are not
applications like `(func1)`.

```rkt
(~> arg func1 func2)
```

is equivalent to:

```rkt
(func2 (func1 arg))
```

### Custom Function Application

This rule applies since the operation is syntically a function application and
not a single identifier.

```rkt
(~> arg (func 11 _ "here" _))
```

is equivalent to:

```rkt
(let ([_ arg])
  (func 11 _ "here" _))
```

## Extras

Various related utilities exist in `pipechain/utils` including:

### `require-safe` and `require-safe*`

Require one element via `(require-safe foo from bar)` or rename it via `(require-safe foo as f from bar)`.

Can require multiple items using `(require-safe foo1 as f1 foo2 as f2 from bar)`.

Easily prefix in an entire module via `(require-safe * as f from foo)` which will expose all elements from as `f:element`.

Can have multiple require specs via `require-safe*` such as `(require-safe* spec1 spec2)` which becomes `(require-safe spec1)` and `(require-safe spec2)`.

Can require for syntax via `(require-safe* args ... #:for-syntax)` which gets translated into `require-safe`.

### `define-proc`

Easily define functions using early return.

```
(define-proc (x)
  (when (invalid? x)
    (return #f))
  (displayln! x)
  #t)
```

### `ifx-utils`

This is a small mini-DSL that provides an RPN calculator-like interface.

