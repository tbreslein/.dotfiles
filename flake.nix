{
  description = "my nixos and nix-darwin based dotfiles";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.05";
    nix-darwin.url = "github:lnl7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    nixpkgs,
    nixpkgs-stable,
    home-manager,
    nix-darwin,
    nix-homebrew,
    ...
  }: let
    settings = {
      sessionPath = [
        "$HOME/bin"
        "$HOME/.local/bin"
      ];
      sessionVariables = rec {
        EDITOR = "nvim";
        VISUAL = EDITOR;
        BROWSER = "brave";
      };
    };

    forAllSystems = function:
      nixpkgs.lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ] (system: function nixpkgs.legacyPackages.${system});
    # mkSystem = hostname: system: let
    #   systemFunc =
    #     if (nixpkgs.lib.hasSuffix "linux" system)
    #     then nixpkgs.lib.nixosSystem
    #     else nix-darwin.lib.darwinSystem;
    #
    #   hmModules =
    #     if (nixpkgs.lib.hasSuffix "linux" system)
    #     then home-manager.nixosModules
    #     else home-manager.darwinModules;
    #
    #   pkgs-stable = import nixpkgs-stable {
    #     inherit system;
    #     config.allowUnfree = true;
    #   };
    #   pkgs = import nixpkgs {
    #     inherit system;
    #     config.allowUnfree = true;
    #   };
    #
    #   specialArgs = {inherit settings pkgs-stable;};
    # in {
    #   "${hostname}" = systemFunc {
    #     inherit system;
    #     inherit pkgs;
    #     inherit specialArgs;
    #     modules = [
    #       (./hosts/. + "/${hostname}/" + ./configuration.nix)
    #       ./modules/system
    #
    #       hmModules.home-manager
    #       {
    #         home-manager = {
    #           useGlobalPkgs = true;
    #           useUserPackages = true;
    #           extraSpecialArgs = specialArgs;
    #           users."${settings.userName}".imports = [./modules/hm (./hosts/. + "${hostname}" + ./home.nix)];
    #         };
    #       }
    #     ];
    #   };
    # };
  in {
    # NOTE:
    # rebuild commands should be
    # - sudo nixos-rebuild switch --flake "$HOME/.dotfiles"
    # - darwin-rebuild switch --flake "$HOME/.dotfiles"
    # nixosConfigurations = nixpkgs.lib.mkMerge [
    #   (mkSystem "kain" "x86_64-linux")
    #   (mkSystem "raziel" "x86_64-linux")
    #   (mkSystem "vorador" "aarch64-linux")
    # ];
    # darwinConfigurations = nixpkgs.lib.mkMerge [
    #   (mkSystem "tommysmbp" "aarch64-darwin")
    # ];
    darwinConfigurations."tommysmbp" = let
      system = "aarch64-darwin";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      pkgs-stable = import nixpkgs-stable {
        inherit system;
        config.allowUnfree = true;
      };
      specialArgs = {inherit settings pkgs-stable;};
    in
      nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        pkgs = import nixpkgs {
          system = "aarch64-darwin";
          config.allowUnfree = true;
        };
        specialArgs = {inherit settings pkgs-stable;};
        modules = [
          ./hosts/tommysmbp/configuration.nix
          ./modules/system

          nix-homebrew.darwinModules.nix-homebrew
          {
            nix-homebrew = {
              enable = true;
              enableRosetta = true;
              user = "tommy";
            };
          }

          home-manager.darwinModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = {inherit settings pkgs-stable;};
              users."tommy".imports = [./modules/hm ./hosts/tommysmbp/home.nix];
            };
          }
        ];
      };

    devShells = forAllSystems (pkgs: {
      default = pkgs.mkShell {
        packages = with pkgs; [
          vim
          nil
          statix
          alejandra
          luajitPackages.lua-lsp
          stylua
        ];
      };
    });
  };
}
