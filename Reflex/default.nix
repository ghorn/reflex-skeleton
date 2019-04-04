(import ./reflex.nix).project ({ pkgs, ... }: {
  # useWarp = true;
  packages = {
    Reflex = ./.;
  };

  shells = {
    ghc = ["Reflex"];
  };
})
