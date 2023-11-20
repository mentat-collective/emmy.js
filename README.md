
# Emmy.js

## Warning

The library is not yet complete even in an initial sense: I have
flood-filled the things that are required to do what is
done [here](https://kloimhardt.github.io/blog/html/sicmutils-as-js-book-part1.html)
in natural JS syntax, but there are many functions in Emmy that are
as yet unwrapped.

For the present, I invite you to consider the test case file `emmyTest.js`
to be the current state of the art for the recommended use of the library.
At present, it contains most of what is in Chapter 1 of SICM with very
little compromise in the source code.

## Goals

Emmy.js is an NPM library form of the [emmy] computer algebra system.
The goals of this project are to support the use of Emmy with
idiomatic JavaScript, and to deliver artifacts allowing Emmy to
be composed with other JS packages using standard frontend techniques.

JavaScript's arrow notation for functions combined with function
argument destructuring provide a coding environment that's almost
as pleasant as the original Clojure or Scheme. (One thing we can't
do is overload operators. That leaves us with unpleasant functions
like `add`, `sub`, `mul` and `div`, but practically everything
else looks comprehensible in infix notation.) Here's an example from
SICM chapter 1:

```clojure
(defn L-uniform-acceleration
  "The Lagrangian of an object experiencing uniform acceleration
  in the negative y direction, i.e. the acceleration due to gravity"
  [m g]
  (fn [[_ [_ y] v]]
    (- (* (/ 1 2) m (g/square v)) (* m g y))))
```

```js
const LUniformAcceleration = (m, g) => ([t, [x, y], v]) => e.sub(
    e.mul(1 / 2, m, e.square(v)),
    e.mul(m, g, y)
)
```

Frankly the best practices for writing SICM-like code in straight
JS are still evolving.

## Obstacles

Since Emmy can be compiled with ClojureScript, you may ask, what more
is needed? ClojureScript makes interacting with JavaScript easy to do
from the Clojure side, but it is not so easy to go the other way.

- Many things that look like functions in Emmy aren't callable in
  JavaScript directly (generic functions like +, -). Functions with
  punctuation in their names get replaced with unsightly names like
  `__GT_infix`. The wrapper library fixes these issues.

- We provide a means of repackaging Emmy structures so that they can
  be indexed like arrays, can destructure into JS argument lists, and
  be called like functions when that is useful.

- Clojure allows creating a symbol very easily by writing `'x`.
  Our library allows `e.symbol(x)` for this purpose, but since that
  is fatiguing, we provide a shortcut that allows the definition
  of several symbols in a scoped context:

  ```js
  e.with_symbols('x y V()', (x, y, V) => {
    // within this scope, x and y are symbols, and V is a
    // literal function (since we appended parentheses to the
    // name in the symbol name list).
  })
  ```
- Symbol renaming conventions:

  - `->foo` becomes `to_foo`
  - `foo->bar` becomes `foo_to_bar`
  - `foo!` becomes `foo_now`
  - `-foo` becomes `minus_foo`
  - `foo-bar` becomes `foo_bar`
  - `foo:bar` becomes `foo_bar` (as well)
  - `foo?` becomes `foo_p`

- Generally speaking, the printed form of an Emmy object in the Node
  REPL is not very useful. The results of `.toString()` are generally
  much better, but these are (naturally) presented in Clojure syntax.
  For the test case library, we consider the composition of `->infix`
  and `simplify` to be the natural form of Emmy output in the JS setting,
  and our test cases work with this representation instead of building
  expected values using clojure primitives.

## Build

TLDR: `npm i && npm run release && npm test`

You can use `npm run watch` to run shadow-cljs in watch mode,
while working on the ClojureScript code.
