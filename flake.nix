{
  description = "Steven-Pinkerton/mood-tracker: A web-app for tracking Moods";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-flake.url = "github:srid/treefmt-flake";
    check-flake.url = "github:srid/check-flake";
    deploy-rs.url = "github:serokell/deploy-rs";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, deploy-rs, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-flake.flakeModule
        inputs.check-flake.flakeModule
      ];
      perSystem = { self', config, pkgs, ... }: {
        haskellProjects.default = {
          packages.mood-tracker.root = ./.;
          buildTools = hp: {
            inherit (pkgs)
              treefmt;
          } // config.treefmt.formatters;
          # overrides = self: super: {}
          hlsCheck.enable = true;
          hlintCheck.enable = true;
        };
        treefmt.formatters = {
          inherit (pkgs)
            nixpkgs-fmt;
          inherit (pkgs.haskellPackages)
            cabal-fmt
            fourmolu;
        };

        nixosConfiguration.mood-tracker = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [ ./configuration.nix ];
        };

        defaultPackage.x86_64-linux = import ./src/Main.hs;

        deploy.nodes.mood-tracker = {
          profiles = {
            system = {
              user = "root";
              sshUser = "root";
              path = deploy-rs.lub.x86_64-linux.activate.nixos self.nixosConfiguration.mood-tracker;
            };
            };
          };
        };
        checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
        packages.default = config.packages.mood-tracker;
      };
    }
