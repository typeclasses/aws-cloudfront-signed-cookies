{ mkDerivation, base, ghc-prim, lib, transformers }:
mkDerivation {
  pname = "transformers-compat";
  version = "0.7.2";
  sha256 = "b62c7304c9f3cbc9463d0739aa85cb9489f217ea092b9d625d417514fbcc9d6a";
  libraryHaskellDepends = [ base ghc-prim transformers ];
  homepage = "http://github.com/ekmett/transformers-compat/";
  description = "A small compatibility shim for the transformers library";
  license = lib.licenses.bsd3;
}
