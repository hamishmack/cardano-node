# our packages overlay
pkgs: _: with pkgs; {
  cardanoNodeHaskellPackages = import ./haskell.nix {
    inherit config
      lib
      stdenv
      haskell-nix
      buildPackages
      haskellNixSource
      ;
  };
  cardanoNodeProfiledHaskellPackages = import ./haskell.nix {
    inherit config
      lib
      stdenv
      haskell-nix
      buildPackages
      haskellNixSource
      ;
    profiling = true;
  };
}
