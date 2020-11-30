# lihh

I was going to try to a Lisp for my programming languages class, but Lisp currently doesn't interest me very much.

so I decided to make something more inspired by lambda calculus stuff, which has interested me for a while but I've had trouble finding a use case for

I kept the Lisp syntax because it's simple, and maybe it'll get me more points because it's similar to something on the rubric idk

## helpful sources
- [Programming with Lambda Calculus](https://hbr.github.io/Lambda-Calculus/lambda2/lambda.html)
- [Fixed-point combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator)

## first version

new features:
- functions
- strings (more like symbols)
	- ideally everything should be a function? but you need somewhere to stop? symbols are like the terminators? am not sure I have no training in this
- laziness
	- required (I think) for recursion to work

| builtin | what it does |
| :-----: | :----------- |
| `\`     | makes function |
| `env`   | evaluates many things; returns last thing (used to modify scope) |
| `!`     | defines something in current scope |
| `=`     | evaluates equality between two strings/symbols (note: is infix for no particular reason) |

example program that outputs `YES!!!`
```
(env
	(! true  (\ a (\ b a)))
	(! false (\ a (\ b b)))

	((true YES!!!) No)
)
```

everything is (should be) lazy (thanks to `Data.Map.Lazy`), so something like `((true yes) (this will crash))` will evaluate to `yes`, while `((false yes) (this will crash))` will crash.

here is a more interesting example, which uses the Y combinator for recursion

```
(env
	-- boolean functions
	(! true  (\ a (\ b a)))
	(! false (\ a (\ b b)))
	(! if (\ cond (\ a (\ b ((cond a) b)))))
	(! showbool (\ bool ((bool True) False)))

	-- pair functions (for making lists)
	(! pair (\ a (\ b (\ getter ((getter a) b)))))
	(! fst (\ pair (pair true)))
	(! snd (\ pair (pair false)))

	-- the Y combinator
	(! Y (\ f ((\ x (f (x x))) (\ x (f (x x))))))

	-- contains (which will be fed into the Y combinator)
	(! contains (\ rec (\ lst (\ thing
		(((if (lst = null))
		   false)
		   (((if ((fst lst) = thing))
		      true)
		      ((rec (snd lst)) thing)
		   )
		)
	))))

	-- contains that works
	(! containsY (Y contains))

	-- make a list and check if it contains apple
	(! lst ((pair apple)  null))
	(! lst ((pair banana) lst))
	(! lst ((pair bread)  lst))
	(! lst ((pair butter) lst))
	(showbool ((containsY lst) apple))
)
```

here I used `null` to terminate the list; it can be anything

### second version wishlist:
- cleaner code
- numbers?
- prefix `=`
- an actual program

## second revision/update: syntactic sugar preprocessor
new features/changes:
- an actual program (run program with `./run.sh source.txt`)
- cleaner code
- list expressions that contain only one list expression are now evaluated to that expression

### cleaner code
to reduce number of parentheses in code, I added a cool new syntax that generates the old basic syntax.

| description | syntax | equivalent basic |
| --- | --- | --- |
| (curried) multivariable function | `\your args here. hello` | `(\ your (\ args (\ here hello)))` |
| left-to-right function application | `(> f a b)` | `((f a) b)` |
| right-to-left function application | `(< f a b)` | `(f (a b))` |

here's a previous example in the new syntax:
```
(env
	-- boolean functions
	(!true     \     a b. a)
	(!false    \     a b. b)
	(!if       \cond a b. (> cond a b))
	(!showbool \bool    . (> bool True False))

	-- pair functions (for making lists)
	(!pair \a b getter. (> getter a b))
	(!fst  \pair. (pair true))
	(!snd  \pair. (pair false))

	-- the Y combinator
	(!Y \f. ((\x. (< f x x)) (\x. (< f x x))))

	-- contains (which will be fed into the Y combinator)
	(!contains \rec lst thing.
		(> if (lst = null) false
			(> if ((fst lst) = thing) true
				(> rec (snd lst) thing)
			)
		)
	)

	-- contains that works
	(!containsY (Y contains))

	-- make a list and check if it contains apple
	(!lst (> pair apple  null))
	(!lst (> pair banana lst))
	(!lst (> pair bread  lst))
	(!lst (> pair butter lst))
	(showbool (> containsY lst apple))
)
```

## third revision/update: printing strings

changelog:
- renamed `env` to `_env` to fit with naming convention
- added top-level `_print`ing
	- `_print`s in the highest level produce IO effects
	- for all practical purposes, the top-level `_env`
	- could technically add nested prints, but imo is better suited to keep weird hacky stuff to `_env`
- added `_concat` to concat symbols into a string (does not produce a symbol, e.g. `(_concat tr ue)` will not further evaluate into the actual `true` function)
- added some special symbols for hard-to-input strings
	- `_space` is `" "`
	- `_nl` is `"\n"`

example program that prints lists (by first generating a string)
```
(_env
	(!true     \     a b. a)
	(!false    \     a b. b)
	(!if       \cond a b. (> cond a b))
	(!showbool \bool    . (> bool True False))

	(!pair \a b getter. (> getter a b))
	(!fst  \pair. (pair true))
	(!snd  \pair. (pair false))

	(!Y \f. ((\x. (< f x x)) (\x. (< f x x))))

	(!spaced \a b. (_concat (_concat a _space) b))

	(!plist (Y (\rec lst.
		(> if (lst = null) null
			(> spaced (fst lst) (> rec (snd lst)))
		)
	)))

	(!lst (> pair apple  null))
	(!lst (> pair banana lst))
	(!lst (> pair bread  lst))
	(!lst (> pair butter lst))

	(_print (plist lst))
)
```

## poor informational background
here is a description of relevant concepts that might be good for someone just discovering these concepts (like I am!!) but not good if you want formal correct stuff

### lambda calculus
lambda calculus is a computing model where everything is a function that can only take one variable. that's basically it!

functions are usually represented like
```
λinput.output
```

so a function that takes something and gives it right back (the 'identity function') would be

```
λx.x
```

though `λ` isn't very typable, so `\` is usually used in code, because it looks like it.

```
\x.x

\x.x(hello) = hello
```

note that `hello` is ideally a function as well (i think)

you can make functions that 'take' multiple variables in a row like so:

```
\x.\y.x
```

the first function takes variable x and returns the second function. the second function takes variable y, and returns x. y is never used

```
\x.\y.x(hello)(world) = hello
```

### encoding stuff with functions

you can do a lot of stuff (everything, in theory [I think]) with functions!

here I will describe how to make lists with functions.

#### 1. pairs
but first, we will need pairs/tuples! we want to have something represent `(a, b)`.

but what we actually want are two functions, `first` and `second` that can be called with a tuple, e.g.

```
first((a, b)) = a
second((a, b)) = b
```

all we have are functions, so a pair has to be a function that returns its first or second value, depending on the function input.

in other words, we want a `make_pair` function that takes two values (the first and second), some input, and then figures out based on the input which of the two values to return.
```
make_pair = \a.\b.\input.???
```

since the only thing `input` can be is a function, the only thing you can really do to create this kind of behavior is to outsource the behavior to `input`:

```
make_pair = \a.\b.\input.input(a)(b)
```

and after thinking for a bit, we can make `first` and `second`!

```
first  = \pair.pair(\a.\b.a)
second = \pair.pair(\a.\b.b)
```

here's an example usage:

```
my_pair = make_pair(hello)(world)

first(my_pair)  => hello
second(my_pair) => world
```

#### 2. lists
you can now make lists as pairs of pairs:

```
(apple, (orange, (banana, bread)))
```

or you could make it backwards:

```
(((apple, orange), banana), bread)
```

neat! a list that's actually just a mess of functions!

you can traverse the list recursively, though it might be hard to tell when to stop, so you can add some special terminating symbol (if you want)

```
(apple, (orange, (banana, (bread, null))))
```