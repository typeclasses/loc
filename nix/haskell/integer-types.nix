{ mkDerivation, base, deepseq, exceptions, hashable, hedgehog
, hspec, hspec-hedgehog, lib, quaalude
}:
mkDerivation {
  pname = "integer-types";
  version = "0.1.2.0";
  sha256 = "37ea06340c904aaf297b0d2224bf6b9d45bdf9742fa7e6c51835f809195251aa";
  libraryHaskellDepends = [ base deepseq hashable quaalude ];
  testHaskellDepends = [
    base deepseq exceptions hashable hedgehog hspec hspec-hedgehog
    quaalude
  ];
  homepage = "https://github.com/typeclasses/integer-types";
  description = "Integer, Natural, and Positive";
  license = lib.licenses.asl20;
}
