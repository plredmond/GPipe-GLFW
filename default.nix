{ mkEnv ? null
, target ? "GPipe-GLFW"
, config ? { allowBroken = true; }
, ...
}:
let
  nixpkgs = import
    (builtins.fetchTarball {
      # latest https://github.com/NixOS/nixpkgs/commits/nixos-20.09 as of Sun 01 Nov 2020 10:25:16 PM UTC
      url = "https://github.com/NixOS/nixpkgs/archive/edb26126d98bc696f4f3e206583faa65d3d6e818.tar.gz";
      sha256 = "1cl4ka4kk7kh3bl78g06dhiidazf65q8miyzaxi9930d6gwyzkci";
    })
    { inherit config; };
  # package set for haskell compiler version
  haskellCompilerPackages = nixpkgs.haskellPackages; # don't care about compiler version
  # override package set to inject project components
  haskellPackages = haskellCompilerPackages.override
    (old: {
      all-cabal-hashes = nixpkgs.fetchurl {
        # latest https://github.com/commercialhaskell/all-cabal-hashes/commits/hackage as of Sun 01 Nov 2020 10:26:50 PM UTC
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/ce6d987d56e5a3f2af4a41a417405e43db36e337.tar.gz";
        sha256 = "17i6hz4063xhxqzdddp38clhm9sccsd9y4wykz65hikhaxhpnwlh";
      };
      overrides = self: super: with nixpkgs.haskell.lib; rec {
        # declare each of the packages in this project
        GPipe = overrideCabal (self.callHackage "GPipe" "2.2.5" { }) {
          jailbreak = true; # relies on an old version of linear
        };
        GPipe-GLFW = self.callCabal2nix "GPipe-GLFW" (nixpkgs.nix-gitignore.gitignoreSource [ ] ./GPipe-GLFW) { };
        Smoketests = self.callCabal2nix "Smoketests" (nixpkgs.nix-gitignore.gitignoreSource [ ] ./Smoketests) { };
      };
    });
  # packages part of this local project
  projectPackages = with haskellPackages; [
    GPipe
    GPipe-GLFW
    Smoketests
  ];
  # derivation to build
  drv = haskellPackages."${target}";
in
if
  (mkEnv != null && mkEnv) || (mkEnv == null && nixpkgs.lib.inNixShell)
then
  drv.env.overrideAttrs
    (old: { nativeBuildInputs = old.nativeBuildInputs ++ [ nixpkgs.ghcid ]; })
else
  drv
