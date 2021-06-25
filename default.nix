{ mkEnv ? null
, target ? "GPipe-GLFW"
, config ? { allowBroken = true; }
, nixpkgs ? import <nixpkgs> { inherit config; }
, ...
}:
let
  # package set for haskell compiler version
  haskellCompilerPackages = nixpkgs.haskellPackages; # don't care about compiler version
  # override package set to inject project components
  haskellPackages = haskellCompilerPackages.override
    (old: {
      overrides = self: super: with nixpkgs.haskell.lib; rec {
        # declare each of the packages in this project
        GPipe = overrideCabal (self.callHackage "GPipe" "2.2.5" { }) {
          jailbreak = true; # relies on an old version of linear
        };
        GPipe-GLFW = overrideCabal (self.callCabal2nix "GPipe-GLFW" (nixpkgs.nix-gitignore.gitignoreSource [ ] ./GPipe-GLFW) { }) {
          passthru = {
            inherit nixpkgs;
            inherit haskellPackages;
            inherit projectPackages;
          };
        };
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
