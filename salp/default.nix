{haskellPackages}:
haskellPackages.mkDerivation {
    pname = "salp";
    version = "0.0.0";
    license = null;
    src = builtins.filterSource (p: t: p != toString ./dist) ./.;
    buildDepends = [
        haskellPackages.base
        haskellPackages.bytestring
        haskellPackages.megaparsec
        haskellPackages.mtl
        haskellPackages.transformers
        haskellPackages.unordered-containers
    ];
}
