{haskellPackages}:
haskellPackages.mkDerivation {
    pname = "acetone";
    version = "0.0.0";
    license = null;
    src = builtins.filterSource (p: t: p != toString ./dist) ./.;
    buildDepends = [
        haskellPackages.base
        haskellPackages.bytestring
        haskellPackages.containers
        haskellPackages.lens
        haskellPackages.mtl
        haskellPackages.transformers
    ];
}
