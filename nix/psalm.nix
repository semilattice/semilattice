{stdenv, php}:
stdenv.mkDerivation rec {
    name = "psalm-${version}";
    version = "3.2.2";
    src = builtins.fetchurl {
        url = "https://github.com/vimeo/psalm/releases/download/${version}/psalm.phar";
        sha256 = "1mx4r39zri5rhlbzj77nzspqif4dba1pw9pqbc0i09vq2kj7rv8l";
    };
    buildInputs = [php];
    phases = ["installPhase"];
    installPhase = ''
        mkdir -p $out/bin $out/share
        cp ${src} $out/share/psalm.phar
        cat <<EOF > "$out/bin/psalm"
        #!/bin/sh
        export PATH="${php}/bin:\$PATH"
        exec php $out/share/psalm.phar "\$@"
        EOF
        chmod +x $out/bin/psalm
    '';
}
