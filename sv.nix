{ mkDerivation, ansi-wl-pprint, attoparsec, base, bifunctors
, bytestring, charset, containers, contravariant, hedgehog, lens
, mtl, parsec, parsers, profunctors, readable, semigroupoids
, semigroups, separated, stdenv, tasty, tasty-hedgehog, tasty-hunit
, text, transformers, trifecta, validation, void
}:
mkDerivation {
  pname = "sv";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    ansi-wl-pprint attoparsec base bifunctors bytestring charset
    containers contravariant lens mtl parsec parsers profunctors
    readable semigroupoids semigroups separated text transformers
    trifecta validation void
  ];
  testHaskellDepends = [
    ansi-wl-pprint base bytestring contravariant hedgehog lens parsers
    semigroupoids semigroups separated tasty tasty-hedgehog tasty-hunit
    text trifecta validation
  ];
  homepage = "https://github.com/qfpl/sv";
  description = "Parse and decode values which are separated, often by commas";
  license = stdenv.lib.licenses.bsd3;
}
