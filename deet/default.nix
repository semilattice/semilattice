{stdenv, phpPackages, postgresql_11}:
stdenv.mkDerivation {
    name = "deet";
    src = ./.;
    buildInputs = [
        phpPackages.composer
        phpPackages.psalm
        postgresql_11
    ];
    phases = ["unpackPhase" "buildPhase" "installPhase"];
    buildPhase = ''
        source build/postgresql.bash
        source build/generate.bash
        source build/compile.bash
    '';
    installPhase = ''
        source build/install.bash
    '';
}
