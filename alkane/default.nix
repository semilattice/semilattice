{stdenv, cargo}:
stdenv.mkDerivation {
    name = "alkane";
    src = ./.;
    buildInputs = [cargo];
    phases = ["buildPhase"];
    buildPhase = ''
        1>&2 echo 'This derivation exists only for use with nix-shell.'
        false
    '';
}
