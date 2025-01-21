{
  inputs = {
    "nixos-24.11".url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, ... }:
    let packageName = "aws-cloudfront-signed-cookies";
    in inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = {
          "nixos-24.11" = import inputs."nixos-24.11" { inherit system; };
        };
        pkgs = nixpkgs."nixos-24.11";
        inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

        combineOverrides = old:
          fold composeExtensions (old.overrides or (_: _: { }));

      in {
        defaultPackage = self.packages.${system}.${packageName};

        devShells.default = let
          project = pkgs.haskell.packages.ghc96.developPackage {
            root = ./aws-cloudfront-signed-cookies;
            name = packageName;
          };

          hls = pkgs.haskell-language-server.override {
            supportedGhcVersions = [ "98" ];
          };
        in pkgs.mkShell {
          inputsFrom = [ project.env ];
          buildInputs = [ pkgs.cabal-install ];
        };

        packages = let

          inherit (pkgs.haskell.lib) dontCheck;

          makeTestConfiguration = let defaultPkgs = pkgs;
          in { pkgs ? defaultPkgs, ghcVersion, overrides ? new: old: { } }:
          let inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
          in (pkgs.haskell.packages.${ghcVersion}.override (old: {
            overrides = combineOverrides old [
              (packageSourceOverrides {
                aws-cloudfront-signed-cookies = ./aws-cloudfront-signed-cookies;
              })
              overrides
            ];

          })).aws-cloudfront-signed-cookies;

        in rec {
          ghc-9-6 = makeTestConfiguration {
            pkgs = nixpkgs."nixos-24.11";
            ghcVersion = "ghc96";
          };
          ghc-9-8 = makeTestConfiguration {
            pkgs = nixpkgs."nixos-24.11";
            ghcVersion = "ghc98";
          };
          all = pkgs.symlinkJoin {
            name = packageName;
            paths = [ ghc-9-6 ghc-9-8 ];
          };
        };
      });
}
