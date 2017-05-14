# GPipe-GLFW

**GPipe-GLFW** is a utility library to enable the use of [GLFW](http://www.glfw.org/)
as the OpenGL window and context handler for [GPipe](#what-is-gpipe).

See examples in the [Smoketests](Smoketests) directory and learn [GPipe](#what-is-gpipe).

Find GPipe-GLFW on
[github](https://github.com/plredmond/GPipe-GLFW),
[hackage](https://hackage.haskell.org/package/GPipe-GLFW), and
[stackage](https://www.stackage.org/package/GPipe-GLFW).

## Changes

See repo tags.

* 1.4.1
    * Split `Wrapped` module to `Window` and `Misc` modules.
    * Don't expose `ErrorCallback`, do expose  the `Error` type for custom error callbacks.
    * Switch from ad-hoc parenting for shared contexts, to the "ancestor" pattern described in [#24](https://github.com/plredmond/GPipe-GLFW/issues/24#issuecomment-299681824)
    * Adjustments to debug logging format.
    * Add smoketest for window close functions & sequential GPipe windows.
    * Bump deps to GPipe-2.2.1.
* 1.4.0
    * Rewrite for new window handling interface. Separate smoke tests to own package.
* 1.3.0
    * Overhaul `Graphics.GPipe.Context.GLFW.Input` to expose most of the functionality in [GLFW Input guide](http://www.glfw.org/docs/latest/input_guide.html).
* 1.2.3
    * [SwiftsNamesake](https://github.com/SwiftsNamesake) bumped version constraints.
    * Add a smoke test and stubs for shared-context tests.
* 1.2.2
    * [grtlr](https://github.com/grtlr) added scroll callback registration.
    * Add a readme to be a good citizen and update documentation.
* 1.2.1
    * [bch29](https://github.com/bch29) refactored and added new GLFW input callback registration functions as well as the `unsafe` module to access the GLFW window directly.
* 1.2
    * [bch29](https://github.com/bch29) exposed more of the underlying GLFW hints.

# What is GPipe?

**GPipe** is a typesafe functional API based on the conceptual model of OpenGL,
but without the imperative state machine.

To learn GPipe, start with the [readme](https://github.com/tobbebex/GPipe-Core#readme).
You could also go directly to [a working example](https://github.com/plredmond/GPipe-Test#readme)
or the tutorials
[Part 1](http://tobbebex.blogspot.se/2015/09/gpu-programming-in-haskell-using-gpipe.html),
[Part 2](http://tobbebex.blogspot.se/2015/09/gpu-programming-in-haskell-using-gpipe_11.html),
[Part 3](http://tobbebex.blogspot.se/2015/10/gpu-programming-in-haskell-using-gpipe.html),
[Part 4](http://tobbebex.blogspot.se/2015/10/gpu-programming-in-haskell-using-gpipe_21.html),
[Part 5](http://tobbebex.blogspot.se/2015/11/gpu-programming-in-haskell-using-gpipe.html).

Find GPipe on
[github](https://github.com/tobbebex/GPipe-Core),
[hackage](https://hackage.haskell.org/package/GPipe), and
[stackage](https://www.stackage.org/package/GPipe).
