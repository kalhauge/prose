{ compiler ? "default"
, pkgs ? fix.nixpkgs {}

, overrides ? {}
, fix-src ? ./nix/fix
, fix ? import fix-src fix // overrides
}:
let
  haskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages."${compiler}";
in
  haskellPackages.developPackage {
     root = pkgs.lib.cleanSourceWith
      { filter = path: type: !(pkgs.lib.hasSuffix ".nix" path);
        src = fix.hercules-ci.gitignoreSource ./.;
      };
    name = "prose";
    source-overrides = {
    };
    overrides = hsuper: hself: { };
    modifier = drv:
      with pkgs.haskell.lib;
      addBuildTools drv (with haskellPackages;
        [ cabal-install ghcid haskell-language-server retrie ])
    ;
  }
