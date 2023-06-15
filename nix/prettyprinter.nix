{ mkDerivation, ansi-wl-pprint, base, base-compat, bytestring
, containers, deepseq, doctest, gauge, lib, mtl, pgp-wordlist
, QuickCheck, quickcheck-instances, random, tasty, tasty-hunit
, tasty-quickcheck, text, transformers
}:
mkDerivation {
  pname = "prettyprinter";
  version = "1.7.1";
  sha256 = "5e6ea6903114fa118fcc359633dfb7ecddecb92c06c853d02a77b72b251f0b45";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base text ];
  testHaskellDepends = [
    base bytestring doctest pgp-wordlist QuickCheck
    quickcheck-instances tasty tasty-hunit tasty-quickcheck text
  ];
  benchmarkHaskellDepends = [
    ansi-wl-pprint base base-compat containers deepseq gauge mtl
    QuickCheck random text transformers
  ];
  homepage = "http://github.com/quchen/prettyprinter";
  description = "A modern, easy to use, well-documented, extensible pretty-printer";
  license = lib.licenses.bsd2;
}
