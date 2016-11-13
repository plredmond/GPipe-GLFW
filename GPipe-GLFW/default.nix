{ mkDerivation, base, GLFW-b, GPipe, stdenv, transformers }:
mkDerivation {
  pname = "GPipe-GLFW";
  version = "1.2.3";
  src = ./.;
  libraryHaskellDepends = [ base GLFW-b GPipe transformers ];
  homepage = "https://github.com/plredmond/GPipe-GLFW";
  description = "GLFW OpenGL context creation for GPipe";
  license = stdenv.lib.licenses.mit;
}
