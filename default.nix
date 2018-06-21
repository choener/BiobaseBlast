with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsSrcSet = (lib.foldl' (s: p: s // (import p).hsSrcSet) {} [
    ../Lib-BiobaseENA
    ../Lib-BiobaseTypes
    ../Lib-BiobaseXNA
    ../Lib-PrimitiveArray
    ../Lib-SciBaseTypes
  ]) // {BiobaseBlast = ./.;};
  hsPkgs = haskellPackages.extend (packageSourceOverrides hsSrcSet);
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.BiobaseBlast ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
      BiobaseENA
      BiobaseTypes
      BiobaseXNA
      PrimitiveArray
      SciBaseTypes
    ];
  };
}
