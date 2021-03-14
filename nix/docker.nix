{ compiler ? "default"
, pkgs ? fix.nixpkgs {}

, overrides ? {}
, fix-src ? ./fix
, fix ? import fix-src fix // overrides
}@input:
let
  smallpkgs = pkgs.haskell.lib.justStaticExecutables (import ../default.nix input);
in pkgs.dockerTools.buildLayeredImage {
  name = "kalhauge/prose";
  tag = "latest";
  contents = [ smallpkgs ];
  config = {
    # Env = [ "LANG=\"en_US.UTF-8\"" "LC_ALL=\"en_US.UTF-8\"" ];
    Cmd = [ "${smallpkgs}/bin/prose" "pandoc" ];
  };
}
