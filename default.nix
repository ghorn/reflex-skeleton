with import <nixpkgs> {};
haskellPackages.developPackage {
  root = ./.;
  name = "skeleton";
}
