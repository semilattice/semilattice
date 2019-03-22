let
    nixpkgs = fetchTarball {
        url = https://github.com/NixOS/nixpkgs/archive/db7be5298809304d55bfc563f2685854baf83aea.tar.gz;
        sha256 = "1y434vmbyil7bgxk2jgwyprqnsxj645n8jv7nd6xq5jz5wir6nyg";
    };
    config = {
        packageOverrides = pkgs: {
            phpPackages = pkgs.phpPackages // {
                psalm = pkgs.callPackage ./psalm.nix {};
            };
        };
    };
in
    {}: import nixpkgs {config = config;}
