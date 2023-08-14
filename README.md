
# Emmy.js

## Warning

This isn't live yet. The code needed to support this is not yet
checked in to the Emmy library, so there's no version we can point
to that will work as a basis. The `deps.edn` file currently points
to a local repository on my machine; if you want to try this before
it ships you'll have to check out the `emmy` branch
`littleredcomputer/js-build` and point your `local/root` target
to your checkout in `deps.edn`.

Since we don't have an emmy version yet, I haven't written the gulp
tasks to npm publish yet.

The library is not yet complete even in an initial sense: I have
flood-filled the things that are required to do what is
done [here](https://kloimhardt.github.io/blog/html/sicmutils-as-js-book-part1.html)
in natural JS syntax, but there are many functions in Emmy that are
as yet unwrapped.

For the present, I invite you to consider the test case file `emmyTest.ts`
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
  `__GT_infix`. The wrapper library fixes this issues.

- We provide a means of repackaging Emmy structures so that they can
  be indexed like arrays, can destructure into JS argument lists, and
  be called like functions when that is useful.

- Clojure allows creating a symbol very easily by writing `'x`.
  Our library allows `e.symbol(x)` for this purpose, but since that
  is fatiguing, we provide a shortcut that allows the definition
  of several symbols in a scoped context:

  ```js
  e.withSymbols('x y V()', (x, y, V) => {
    // within this scope, x and y are symbols, and V is a
    // literal function (since we appended parentheses to the
    // name in the symbol name list).
  })
  ```

- Generally speaking, the printed form of an Emmy object in the Node
  REPL is not very useful. The results of `.toString()` are generally
  much better, but these are (naturally) presented in Clojure syntax.
  For the test case library, we consider the composition of `->infix`
  and `simplify` to be the natural form of Emmy output in the JS setting,
  and our test cases work with this representation instead of building
  expected values using clojure primitives.

## Yes, implicit `any`

Setting `noImplicitAny: false` in the `tsconfig.json` file is
frowned upon, but in my (present) judgment it is the correct
choice for this library. Because Emmy is architected around
generic functions, the signature of most library functions is
`(...any[]): any`. Nothing wrong with that, but when you are writing
your own functions, especially with arrow notation, being
badgered by the linter to append `:any` to each and every function
argument is annoying.

Many emmy functions could be given stronger types. But it is
not obvious to me how to represent the way that Emmy accounts
for objects' types in TypeScript's type language. Even if we
did so, forcing the user to deal with (e.g.) warnings coming from the
use of union types by making type assertions is foreign to the
Emmy style as developed in Sussman and Wisdom's work.

## Build

We use a gulpfile to arrange for the compilation of the emmy source
code and the transpilation of the TypeScript into JavaScript.

Start the setup with `npm i` to install all the dependencies and
development tools we use. Then `npx gulp` should arrange for all the
assets to be built in the `build` directory.

If you want to develop simultaneously on both the `emmy` and `emmy.js`
repositories, you can `npx gulp watch_cljs` to watch the Clojure side.
To watch the TypeScript side, I use the `tsc watch` command from
within VS code. If you want to hack the JS without VS Code let me know
and I can add a watcher for the TS compiler that gulp could start,
but the watcher and linting features of VS code for TypeScript are
pretty compelling, so I recommend starting there.
