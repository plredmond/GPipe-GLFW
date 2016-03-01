# GPipe-GLFW

**GPipe-GLFW** is a utility library to enable the use of
[GLFW](http://www.glfw.org/) as the OpenGL window and context handler for
GPipe. Resources for learning GPipe are listed in the next section.

Find GPipe-GLFW on
[github](https://github.com/plredmond/GPipe-GLFW),
[stackage](https://www.stackage.org/package/GPipe-GLFW), and
[hackage](https://hackage.haskell.org/package/GPipe-GLFW).

## GPipe

**GPipe** is a typesafe functional API based on the conceptual model of OpenGL,
but without the imperative state machine.

To learn GPipe, start with the
[readme](https://github.com/tobbebex/GPipe-Core#readme). You could also go
directly to [a working example](https://github.com/plredmond/GPipe-Test)
or the tutorials
[Part 1](http://tobbebex.blogspot.se/2015/09/gpu-programming-in-haskell-using-gpipe.html),
[Part 2](http://tobbebex.blogspot.se/2015/09/gpu-programming-in-haskell-using-gpipe_11.html),
[Part 3](http://tobbebex.blogspot.se/2015/10/gpu-programming-in-haskell-using-gpipe.html),
[Part 4](http://tobbebex.blogspot.se/2015/10/gpu-programming-in-haskell-using-gpipe_21.html),
[Part 5](http://tobbebex.blogspot.se/2015/11/gpu-programming-in-haskell-using-gpipe.html).

Find GPipe on
[github](https://github.com/tobbebex/GPipe-Core),
[stackage](https://www.stackage.org/package/GPipe), and
[hackage](https://hackage.haskell.org/package/GPipe).

# Changes

* 1.2.2
    * [grtlr](https://github.com/grtlr) added scroll callback registration.
    * Add a readme to be a good citizen and update documentation.
* 1.2.1
    * [bch29](https://github.com/bch29) refactored and added new GLFW input callback registration functions as well as the `unsafe` module to access the GLFW window directly.
* 1.2
    * [bch29](https://github.com/bch29) exposed more of the underlying GLFW hints.
