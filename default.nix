{ mkDerivation, aeson, aeson-pretty, base-noprelude, bytestring
, checkers, containers, hspec, HUnit, lens, mtl, protolude
, QuickCheck, stdenv
}:
mkDerivation {
  pname = "apply-lens";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base-noprelude bytestring containers lens mtl
    protolude
  ];
  executableHaskellDepends = [
    aeson aeson-pretty base-noprelude bytestring containers lens mtl
    protolude
  ];
  testHaskellDepends = [
    aeson aeson-pretty base-noprelude bytestring checkers containers
    hspec HUnit lens mtl protolude QuickCheck
  ];
  homepage = "https://github.com/githubuser/ #readme";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
