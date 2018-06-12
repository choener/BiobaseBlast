with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsPkgs = haskellPackages.extend (packageSourceOverrides {
    BiobaseBlast = ./.;
    BiobaseENA = ../Lib-BiobaseENA;
    BiobaseTypes = ../Lib-BiobaseTypes;
    BiobaseXNA = ../Lib-BiobaseXNA;
    DPutils = ../Lib-DPutils;
    ForestStructures = ../Lib-ForestStructures;
    OrderedBits = ../Lib-OrderedBits;
    PrimitiveArray = ../Lib-PrimitiveArray;
    SciBaseTypes = ../Lib-SciBaseTypes;
  });
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.BiobaseBlast ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
      BiobaseENA BiobaseTypes BiobaseXNA
      DPutils
      ForestStructures
      OrderedBits
      PrimitiveArray
      SciBaseTypes
    ];
  };
}
