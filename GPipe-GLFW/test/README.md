## Running smoke-tests

There's no official test suite in this project.
The executable `gipe-glfw-smoketests` runs a brief 3D demo which requires a correctly configured OpenGL environment.

When set up as a Cabal test-suite, `gpipe-glfw-smoketests` fails under the `nix-build` environment.
Suggestions for improving this part of the codebase are welcome.

---

`cabal run gipe-glfw-smoketests`

Or..

`stack exec gipe-glfw-smoketests`
