{
  description = "A Haskell project";

  inputs.hix.url = "github:tek/hix?ref=0.8.0";

  outputs =
    { hix, ... }:
    hix.lib.flake {
      hackage.versionFile = "ops/version.nix";

      compiler = "ghc910";

      envs.dev = {
        haskellTools =
          ghc: with ghc; [
            fourmolu
          ];
      };

      cabal = {
        license = "BSD-2-Clause-Patent";
        license-file = "LICENSE";
        author = "James Burton";
        ghc-options = [
          "-Wall"
          "-Wcompat"
          "-Widentities"
          "-Wredundant-constraints"
          "-Wmissing-deriving-strategies"
          "-Wunused-packages"
          "-Winvalid-haddock"
          "-Wredundant-bang-patterns"
          "-Woperator-whitespace"
          "-Wredundant-strictness-flags"
          "-O1"
        ];

        language = "GHC2021";
      };

      packages.challenges = {
        src = ./.;
        cabal.meta.synopsis = "A Haskell project";

        library = {
          enable = true;
          dependencies = [
            "containers"
            "unordered-containers"
            "mtl"
            "vector"
            "random"
          ];
        };

        executable.enable = false;

        test = {
          enable = false;
          dependencies = [
          ];
        };

      };
    };
}
