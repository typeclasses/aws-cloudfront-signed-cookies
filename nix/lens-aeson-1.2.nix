{ mkDerivation, aeson, attoparsec, base, bytestring, lens, lib
, scientific, text, text-short, unordered-containers, vector
}:
mkDerivation {
  pname = "lens-aeson";
  version = "1.2";
  sha256 = "91cf0929ceee8b21d7d4254abde6a1e6b90a8b4cb67fc9043fdef661d7f020c2";
  libraryHaskellDepends = [
    aeson attoparsec base bytestring lens scientific text text-short
    unordered-containers vector
  ];
  homepage = "http://github.com/lens/lens-aeson/";
  description = "Law-abiding lenses for aeson";
  license = lib.licenses.mit;
}
