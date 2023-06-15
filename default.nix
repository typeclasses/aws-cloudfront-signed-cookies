let

sources = import ./nix/sources.nix;
nixos-22-05 = import sources."nixos-22.05" {};
nixos-22-11 = import sources."nixos-22.11" {};
inherit (nixos-22-11) haskell lib symlinkJoin;
inherit (lib) fold composeExtensions concatMap attrValues;

combineOverrides = old:
    fold composeExtensions (old.overrides or (_: _: { }));

sourceOverrides = haskell.lib.packageSourceOverrides {
    aws-cloudfront-signed-cookies = ./aws-cloudfront-signed-cookies;
};

ghc."8.10" = nixos-22-05.haskell.packages.ghc8107.override (old: {
    overrides = combineOverrides old [
        sourceOverrides
        (new: old: {
            lens-aeson = new.callPackage ./nix/lens-aeson-1.2.nix {};
        })
    ];
});

ghc."9.0" = nixos-22-11.haskell.packages.ghc90.override (old: {
    overrides = combineOverrides old [
        sourceOverrides
        (new: old: {
            lens-aeson = new.callPackage ./nix/lens-aeson-1.2.nix {};
        })
    ];
});

ghc."9.2" = nixos-22-11.haskell.packages.ghc92.override (old: {
    overrides = combineOverrides old [
        sourceOverrides
        (new: old: {
            lens-aeson = new.callPackage ./nix/lens-aeson-1.2.nix {};
        })
    ];
});

ghc."9.4" = nixos-22-11.haskell.packages.ghc94.override (old: {
    overrides = combineOverrides old [ sourceOverrides ];
});

in

symlinkJoin {
    name = "aws-cloudfront-signed-cookies";
    paths = concatMap (x: [x.aws-cloudfront-signed-cookies]) (attrValues ghc);
} // {
    inherit ghc;
    pkgs = nixos-22-11;
}
