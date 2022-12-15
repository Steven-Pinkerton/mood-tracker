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

        packages.default = self'.packages.mood-tracker;
      };
      flake = {
        nixosConfigurations.mood-tracker = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./configuration.nix
            ({ config, pkgs, ... }: {
              imports = [ "${nixpkgs}/nixos/modules/virtualisation/openstack-config.nix" ];
              environment.systemPackages = [ self.packages.x86_64-linux.mood-tracker ];
            }
            )
          ];
        };

        deploy.nodes.mood-tracker = {
          hostname = "193.16.42.121";
          profiles = {
            system = {
              user = "root";
              sshUser = "root";
              path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.mood-tracker;
              #We also set this setting to true, as if it is an issue with the response time, building the system localy to the remote may resolve this.
              remoteBuild = true;
              #We set this to false in order to test the hypothesis that this is a known error. Auto-rollback still applies
              magicRollback = false;
            };
          };
        };
        checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
      };
    };
}
