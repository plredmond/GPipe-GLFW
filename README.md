# GPipe-GLFW

**GPipe-GLFW** is a utility library to enable the use of [GLFW](http://www.glfw.org/)
as the OpenGL window and context handler for GPipe.
[What is GPipe?](https://github.com/plredmond/GPipe-GLFW#what-is-gpipe).

Find GPipe-GLFW on
[github](https://github.com/plredmond/GPipe-GLFW),
[hackage](https://hackage.haskell.org/package/GPipe-GLFW), and
[stackage](https://www.stackage.org/package/GPipe-GLFW).

## Build instructions

* Cabal

  * With Nix: [Add this project to `~/.nixpkgs/config.nix`](https://nixos.org/nixpkgs/manual/#how-to-build-projects-that-depend-on-each-other) and then use either `nix-shell` or `nix-build`.
    * `nix-shell "<nixpkgs>" -A haskellPackages.GPipe-Test.env` will open a shell where `cabal build` and any other Cabal commands should work.
    * `nix-build "<nixpkgs>" -A haskellPackages.GPipe-Test`
  * Without Nix: Install the system package dependencies listed in `stack.yaml` using your system's package manager. It is recommended that you use sandboxes to allow Cabal's resolver to pick Haskell dependencies.

* Stack

  * With Nix: Use `stack build` and the [system package dependencies listed in `stack.yaml` will be fetched for a build shell](https://github.com/commercialhaskell/stack/blob/master/doc/nix_integration.md).
  * Without Nix: Install the system package dependencies listed in `stack.yaml` using your system's package manager. Then build with `stack build`.

## Changes

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
