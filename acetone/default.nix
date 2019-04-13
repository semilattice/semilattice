{haskellPackages}:
haskellPackages.mkDerivation {
    pname = "acetone";
    version = "0.0.0";
    license = null;
    src = builtins.filterSource (p: t: p != toString ./dist) ./.;
    buildDepends = [
        haskellPackages.array
        haskellPackages.base
        haskellPackages.bytestring
        haskellPackages.containers
        haskellPackages.file-embed
        haskellPackages.lens
        haskellPackages.mtl
        haskellPackages.transformers
    ];
    buildTools = [
        haskellPackages.alex
        haskellPackages.happy
    ];
}
