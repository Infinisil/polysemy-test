let

  pkgs = import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/54f385241e6649128ba963c10314942d73245479";
    sha256 = "0bd4v8v4xcdbaiaa59yqprnc6dkb9jv12mb0h5xz7b51687ygh9l";
  }) {
    config = {};
    overlays = [];
  };
  inherit (pkgs) lib;
  hlib = pkgs.haskell.lib;

  hpkgs = pkgs.haskell.packages.ghc865.override (old: {
    overrides = lib.composeExtensions (old.overrides or (self: super: {})) (self: super: {
      poly = self.callCabal2nix "poly" (lib.sourceByRegex ./. [
        "^.*\\.hs$"
        "^.*\\.cabal$"
      ]) {};

      polysemy-plugin = hlib.unmarkBroken (hlib.dontCheck super.polysemy-plugin);
    });
  });
  
  env = hpkgs.shellFor {
    packages = p: [ p.poly ];
    nativeBuildInputs = [ hpkgs.cabal-install ];
  };
in hpkgs.poly // {
  inherit env hpkgs;
}
