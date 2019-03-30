{pkgs ? import ./nix/pkgs.nix {}}:
let
    withGhcid = super:
        super.env.overrideAttrs (p: {
            nativeBuildInputs = p.nativeBuildInputs ++ [
                pkgs.haskellPackages.cabal-install
                pkgs.haskellPackages.ghcid
            ];
        });
in
    rec {
        acetone = pkgs.callPackage ./acetone {};
        acetoneGhcid = withGhcid acetone;

        deet = pkgs.callPackage ./deet {};

        rain = pkgs.callPackage ./rain {};

        salp = pkgs.callPackage ./salp {};
        salpGhcid = withGhcid salp;
    }
