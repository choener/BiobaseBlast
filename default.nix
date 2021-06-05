{ mkDerivation, aeson, attoparsec, base, binary, BiobaseENA
, BiobaseTypes, BiobaseXNA, bytestring, cereal, containers, deepseq
, directory, file-embed, filepath, lens, log-domain, mtl
, PrimitiveArray, SciBaseTypes, split, stdenv, tasty
, tasty-quickcheck, tasty-silver, tasty-th, text
, unordered-containers, vector, vector-th-unbox
}:
mkDerivation {
  pname = "BiobaseBlast";
  version = "0.3.3.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base binary BiobaseENA BiobaseTypes BiobaseXNA
    bytestring cereal containers deepseq directory file-embed lens
    log-domain mtl PrimitiveArray SciBaseTypes text
    unordered-containers vector vector-th-unbox
  ];
  testHaskellDepends = [
    base bytestring containers filepath split tasty tasty-quickcheck
    tasty-silver tasty-th text
  ];
  homepage = "https://github.com/choener/BiobaseBlast";
  description = "BLAST-related tools";
  license = stdenv.lib.licenses.gpl3;
}
