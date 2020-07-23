{ config ? { /*allowBroken = true;*/ }, ... }:
let
  # fetch pinned version of nixpkgs
  nixpkgs = import (
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs-channels/archive/1a92d0abfcdbafc5c6e2fdc24abf2cc5e011ad5a.tar.gz";
      sha256 = "1f9ypp9q7r5p1bzm119yfg202fbm83csmlzwv33p1kf76m2p7mwd";
    }
  ) { inherit config; };
  # override haskell compiler version, add and override dependencies in nixpkgs
  haskellPackages = nixpkgs.haskellPackages.override (
    old: {
      all-cabal-hashes = nixpkgs.fetchurl {
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/0bd3bb7eb8c998872a4b1a0ee83d8c619228ac2a.tar.gz";
        sha256 = "00x1830xqlpadc8zyn90yw6h19ir55l8nz20i36j0aky0c1dgkfm";
      };
      overrides = self: super: with nixpkgs.haskell.lib; rec {
        GPipe = self.callHackage "GPipe" "2.2.5" {};
      };
    }
  );
  # function to bring devtools in to a package environment
  devtools = old: { nativeBuildInputs = old.nativeBuildInputs ++ [ nixpkgs.cabal-install nixpkgs.ghcid ]; }; # ghc and hpack are automatically included
  # ignore files specified by gitignore in nix-build
  source = nixpkgs.nix-gitignore.gitignoreSource [] ./.;
  # use overridden-haskellPackages to call gitignored-source
  drv = haskellPackages.callCabal2nix "GPipe-GLFW" source {};
in
if nixpkgs.lib.inNixShell then drv.env.overrideAttrs devtools else drv
