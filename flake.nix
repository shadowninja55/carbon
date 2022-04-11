{
  inputs = {
    nixpkgs.url = "nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    effectful = {
      url = "github:haskell-effectful/effectful";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, effectful }:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = final.lib.composeExtensions prev.haskell.packageOverrides (hsFinal: hsPrev: {
            carbon = hsFinal.callCabal2nix "carbon" ./. { };
          });
        };
      };
    in
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ overlay ];
          };
          inherit (pkgs.lib) composeExtensions;
          hs = pkgs.haskell.packages.ghc922.override {
            overrides = composeExtensions pkgs.haskell.packageOverrides (final: prev: {
              optparse-applicative = prev.optparse-applicative_0_17_0_0;
              effectful-core = final.callCabal2nix "effectful-core" "${effectful}/effectful-core" { };
              effectful = final.callCabal2nix "effectful" "${effectful}/effectful" { };
            });
          };
        in
        rec {
          packages = rec {
            carbon = hs.carbon;
            default = carbon;
          };

          apps = rec {
            carbon = flake-utils.lib.mkApp { drv = packages.carbon; };
            default = carbon;
          };

          devShells.default = hs.shellFor {
            packages = hsPkgs: with hsPkgs; [ carbon ];
            nativeBuildInputs = with hs; [
              cabal-install
              haskell-language-server
            ];
          };
        }) // {
      overlays.default = overlay;
    };
}
