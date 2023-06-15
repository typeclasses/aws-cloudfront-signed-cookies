{
  inputs = {
    "nixos-23.05".url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, ... }:
    let packageName = "aws-cloudfront-signed-cookies";
    in inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = {
          "nixos-23.05" = import inputs."nixos-23.05" { inherit system; };
        };
        pkgs = nixpkgs."nixos-23.05";
        inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

        combineOverrides = old:
          fold composeExtensions (old.overrides or (_: _: { }));

      in
      {
        defaultPackage = self.packages.${system}.${packageName};

        devShells.default =
          let
            project = pkgs.haskell.packages.ghc96.developPackage {
              root = ./aws-cloudfront-signed-cookies;
              name = packageName;
            };

            hls = pkgs.haskell-language-server.override {
              supportedGhcVersions = [ "96" ];
            };
          in
          pkgs.mkShell {
            inputsFrom = [ project.env ];
            buildInputs = [ hls pkgs.cabal-install ];
          };

        packages = {
          testConfigurations =
            let

              inherit (pkgs.haskell.lib) dontCheck;

              makeTestConfiguration =
                let defaultPkgs = pkgs;
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

            in
            rec {
              ghc-9-0 = makeTestConfiguration {
                pkgs = nixpkgs."nixos-23.05";
                ghcVersion = "ghc90";
              };
              ghc-9-2 = makeTestConfiguration {
                pkgs = nixpkgs."nixos-23.05";
                ghcVersion = "ghc92";
              };
              ghc-9-4 = makeTestConfiguration {
                pkgs = nixpkgs."nixos-23.05";
                ghcVersion = "ghc94";
              };
              ghc-9-6 = makeTestConfiguration {
                pkgs = nixpkgs."nixos-23.05";
                ghcVersion = "ghc96";
                overrides = new: old: {
                  optparse-applicative = dontCheck (new.callPackage ./nix/optparse-applicative.nix { });
                  prettyprinter = dontCheck (new.callPackage ./nix/prettyprinter.nix { });
                  prettyprinter-ansi-terminal = dontCheck (new.callPackage ./nix/prettyprinter-ansi-terminal.nix { });
                  transformers-compat = dontCheck (new.callPackage ./nix/transformers-compat.nix { });
                };
              };
              all = pkgs.symlinkJoin {
                name = packageName;
                paths = [ ghc-9-0 ghc-9-2 ghc-9-4 ghc-9-6 ];
              };
            };
        };
      });
}
