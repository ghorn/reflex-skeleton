(import ./reflex.nix).project ({ pkgs, ... }: {
  # useWarp = true;
  packages = {
    skeleton = ./.;
  };

  shells = {
    ghc = ["skeleton"];
  };
})
