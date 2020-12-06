<style>code { font-family: Consolas }</style>

jh7qbe

----------------

lihh is a Haskell implementation of lambda calculus that uses Lisp-like syntax.


# files
| file | contents |
| ---- | -------- |
| [`./Lihh.hs`](./Lihh.hs) | core interpreter |
| [`./Cool.hs`](./Cool.hs) | extended-to-core/basic-syntax-transformer |
| [`./run.sh`](./run.sh) | script that removes comments, pipes result into `Cool.hs`, and pipes result into `Lihh.hs`. can be used to run examples |
| [`./examples/num.txt`](./examples/num.txt) | example of church numerals in lihh |
| [`./examples/mfr.txt`](./examples/mfr.txt) | example of lists, map, filter, and reduce (foldl) in lihh |
| [`./DEVLOG.html`](./DEVLOG.html) | notes I wrote while developing lihh |

# overview of language
lihh has three primitives:

1. functions
2. symbols
3. strings

all functions have a single parameter (though there is a shorthand for multivariable curried functions in the 'extended' syntax).

functions are not recursive, but recursion can be achieved with the Y combinator (see [`./examples/mfr.txt`](./examples/mfr.txt) or [`./DEVLOG.html`](./DEVLOG.html) for some examples)

symbols are basically like strings, except if they are bound to something (e.g. a function) in the current scope, they evaluate to that something.

strings are strings; they only evaluate to strings. (basically only exist as the output of `_concat` for printing purposes)

that's basically everything; there are no built-in booleans, numbers, lists, etc., though they can be built from functions (see examples for more detail)

# syntax guide

## core
| builtin | what it does | example |
| :-----: | :----------- | :------ |
| `\`     | makes function (note: not valid in extended syntax) | `(\ a a)` (identity function)
| `!`     | produces a 'scope effect' that binds a symbol to something | `(! id (\ a a))` |
| `_print` | produces an 'IO effect' that outputs something to console | `(_print hello)` |
| `_env`   | collects scope and IO effects | `(_env (! id (\ a a)) (_print (id hello)) (_print world))` |
| `=`     | evaluates equality between two symbols; produces `true` or `false` (note: is infix and not curried) | `(hello = world)` |

## string stuff
| builtin | what it does | example |
| :-----: | :----------- | :------ |
| `_concat` | returns the concatanation of two expressions (note: not curried) | `(_print (_concat pine (_concat ap ple)))` |
| `_nl` | newline character | `(_print _nl)` |
| `_space` | space character | `(_print (_concat (_concat hello _space) world))` |
| `_empty` | empty string | `(_print _empty)` |

## extended syntax
`Lihh.hs` accepts only the stuff above, but that syntax uses a ton of parentheses. You can use this alternate syntax and use `Cool.hs` to transform it into `Lihh.hs`-readable syntax.

| description | syntax | equivalent basic |
| --- | --- | --- |
| (curried) multivariable function | `\your args here. hello` | `(\ your (\ args (\ here hello)))` |
| left-to-right function application | `(> f a b)` | `((f a) b)` |
| right-to-left function application | `(< f a b)` | `(f (a b))` |

# examples
though lihh might not seem very impressive, it's quite capable of cool things with not much code.

[`./examples/`](./examples/) contains some examples written in extended syntax. they can be run as-is with `test.sh`.

- [`./examples/num.txt`](./examples/num.txt): church numerals
- [`./examples/mfr.txt`](./examples/mfr.txt): lists: map, filter, and reduce (foldl)
	- also briefly uses some boolean stuff

# major resources (links)
not comprehensive

- [Lambda calculus conceptual stuff](https://hbr.github.io/Lambda-Calculus/lambda2/lambda.html)
- [Y combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator)
- [lis.py](http://www.norvig.com/lispy.html)
- [Simple Parsec examples](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing)