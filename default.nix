{ reflex-commit ? "9e306f72ed0dbcdccce30a4ba0eb37aa03cf91e3" }:

let reflex-platform = builtins.fetchTarball "https://github.com/reflex-frp/reflex-platform/archive/${reflex-commit}.tar.gz"; in

(import reflex-platform {}).project ({ pkgs, ... }:

with pkgs.haskell.lib;

{

  name = "Seis8s";

  packages = {
    seis8s = ./.;
  };

  shells = {
    ghc = ["seis8s"];
    ghcjs = ["seis8s"];
  };

  android = {};

  overrides = self: super: {
    #       lens = self.callHackage "lens" "4.15.4" {}; # saving this example in case we need it later

    Glob = dontCheck super.Glob;

    seis8s = dontCheck (dontHaddock (appendConfigureFlags super.seis8s ["--ghcjs-options=-DGHCJS_BROWSER" "--ghcjs-options=-O2" "--ghcjs-options=-dedupe" "--ghcjs-options=-DGHCJS_GC_INTERVAL=60000"]));

    base-compat-batteries = dontCheck super.base-compat-batteries;

    text-show = dontCheck super.text-show;

    text-short = dontCheck super.text-short;

    criterion = dontCheck super.criterion;


    musicw =
     # dontHaddock (self.callCabal2nix "musicw" ../MusicW {});
     dontHaddock (self.callCabal2nix "musicw" (pkgs.fetchFromGitHub {
      owner = "dktr0";
      repo = "musicw";
      sha256 = "0j5xxy3ry4nwl5bamhrkalgnz8g77bs7ck965mfciyc4k9b66x5i";
      rev = "4adc14978f5466829f744bb8b4217ef2d26b31fa";

    }) {});

    haskellish = dontHaddock # (self.callCabal2nix "haskellish" ../haskellish {});
     (self.callCabal2nix "haskellish" (pkgs.fetchFromGitHub {
      owner = "dktr0";
      repo = "Haskellish";
      sha256 = "03sv69hnav1p8rd6i301kirx4anm5f402is4n7bxmjjqi7br5hna";
      rev = "75cd924f8699da352ef4d441f35a18ee53d598b0";
     }) {});

    tempi = dontHaddock (self.callCabal2nix "tempi" (pkgs.fetchFromGitHub {
        owner = "dktr0";
        repo = "tempi";
        sha256 = "0z4fjdnl7riivw77pl8wypw1a98av3nhpmw0z5g2a1q2kjja0sfp";
        rev = "9513df2ed323ebaff9b85b72215a1e726ede1e96";
     }) {});

  };

})
