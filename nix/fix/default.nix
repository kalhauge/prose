self:
{
  nixpkgs = import ./nixpkgs.nix;
  hspec-hedgehog = import ./hspec-hedgehog.nix;
  hercules-ci = (self.nixpkgs {}).callPackage (import ./hercules-ci.nix) {};
}
