{
  inputs = {
    "nixos-22.11".url = "github:NixOS/nixpkgs/nixos-22.11";
    "nixos-23.05".url = "github:NixOS/nixpkgs/nixos-23.05";
    "nixos-23.11".url = "github:NixOS/nixpkgs/nixos-23.11";
    "nixos-unstable".url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, ... }:
    let packageName = "ascii-char";
    in inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = {
          "nixos-22.11" = import inputs."nixos-22.11" { inherit system; };
          "nixos-23.05" = import inputs."nixos-23.05" { inherit system; };
          "nixos-23.11" = import inputs."nixos-23.11" { inherit system; };
          "nixos-unstable" = import inputs."nixos-unstable" { inherit system; };
        };
        pkgs = nixpkgs."nixos-22.11";
        project = pkgs.haskellPackages.developPackage {
          root = ./ascii-char;
          name = packageName;
        };
        inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

        combineOverrides = old:
          fold composeExtensions (old.overrides or (_: _: { }));

      in {
        defaultPackage = self.packages.${system}.${packageName};

        packages = {
          "${packageName}" = project;

          testConfigurations = let

            inherit (pkgs.haskell.lib) dontCheck;

            makeTestConfiguration = let defaultPkgs = pkgs;
            in { pkgs ? defaultPkgs, ghcVersion, overrides ? new: old: { } }:
            let inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
            in (pkgs.haskell.packages.${ghcVersion}.override (old: {
              inherit (nixpkgs.nixos-unstable) all-cabal-hashes;
              overrides = combineOverrides old [
                (packageSourceOverrides { ascii-char = ./ascii-char; })
                overrides
              ];

            })).ascii-char;

          in rec {
            ghc-9-0 = makeTestConfiguration {
              pkgs = nixpkgs."nixos-22.11";
              ghcVersion = "ghc90";
            };
            ghc-9-2 = makeTestConfiguration {
              pkgs = nixpkgs."nixos-22.11";
              ghcVersion = "ghc92";
            };
            ghc-9-4 = makeTestConfiguration {
              pkgs = nixpkgs."nixos-22.11";
              ghcVersion = "ghc94";
            };
            ghc-9-6 = makeTestConfiguration {
              ghcVersion = "ghc96";
              pkgs = nixpkgs."nixos-23.05";
            };
            ghc-9-8 = makeTestConfiguration {
              ghcVersion = "ghc98";
              pkgs = nixpkgs."nixos-23.11";
              overrides = new: old: {
                hspec = new.callHackage "hspec" "2.11.7" { };
                hspec-core = dontCheck (new.callHackage "hspec-core" "2.11.7" { });
                hspec-discover = new.callHackage "hspec-discover" "2.11.7" { };
                hspec-expectations = new.callHackage "hspec-expectations" "0.8.4" { };
                hspec-meta = new.callHackage "hspec-meta" "2.11.7" { };
                tagged = new.callHackage "tagged" "0.8.8" { };
              };
            };
            all = pkgs.symlinkJoin {
              name = packageName;
              paths = [ ghc-9-0 ghc-9-2 ghc-9-4 ghc-9-6 ghc-9-8 ];
            };
          };
        };
      });
}
