{ pkgs }:

let
  inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

  hls = pkgs.haskell-language-server.override {
    supportedGhcVersions = [ "96" ];
  };

  combineOverrides = old:
    fold composeExtensions (old.overrides or (_: _: { }));

  testConfigurations =
    let
      makeTestConfiguration = { ghcVersion, overrides ? new: old: { } }:
        let
          inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
        in
        (pkgs.haskell.packages.${ghcVersion}.override (old: {
          overrides =
            combineOverrides old [
              (packageSourceOverrides { loc = ../loc; })
              overrides
            ];
        })).loc;
    in
    rec {
      ghc-9-2 = makeTestConfiguration {
        ghcVersion = "ghc92";
      };
      ghc-9-4 = makeTestConfiguration {
        ghcVersion = "ghc94";
      };
      ghc-9-6 = makeTestConfiguration {
        ghcVersion = "ghc96";
        overrides = new: old: {
          ascii-char = new.callPackage ./haskell/ascii-char.nix { };
        };
      };
      all = pkgs.symlinkJoin {
        name = "loc-tests";
        paths = [ ghc-9-2 ghc-9-4 ghc-9-6 ];
      };
    };

in
{

  packages = { inherit testConfigurations; };

  devShells.default = pkgs.mkShell {
    inputsFrom = [ testConfigurations.ghc-9-6.env ];
    buildInputs = [ hls pkgs.cabal-install ];
  };

}
