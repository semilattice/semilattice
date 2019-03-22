{stdenv, gnu-cobol}:
stdenv.mkDerivation {
    name = "rain";
    src = ./.;
    buildInputs = [
        gnu-cobol
    ];
    phases = [
        "unpackPhase"
        "buildPhase"
        "installPhase"
        "fixupPhase"
    ];
    buildPhase = ''
        bin() {
            cobc -O2 -c -x -o "$1.o" "$1"
        }

        bin 'src/rain.cob'

        gcc -o 'rain' 'src/rain.cob.o' -lcob
    '';
    installPhase = ''
        mkdir -p "$out/bin"
        mv 'rain' "$out/bin"
    '';
}
