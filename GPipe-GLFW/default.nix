{ mkDerivation, async, base, exception-transformers, GLFW-b, GPipe
, stdenv, transformers
}:
mkDerivation {
  pname = "GPipe-GLFW";
  version = "1.3.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base exception-transformers GLFW-b GPipe transformers
  ];
  executableHaskellDepends = [
    base exception-transformers GPipe transformers
  ];
  homepage = "https://github.com/plredmond/GPipe-GLFW";
  description = "GLFW OpenGL context creation for GPipe";
  license = stdenv.lib.licenses.mit;
}
