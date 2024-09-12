{
  description = "my nixos and nix-darwin based dotfiles";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.05";
    darwin.url = "github:lnl7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    nixpkgs,
    nixpkgs-stable,
    home-manager,
    darwin,
    ...
  }: let
    settings = rec {
      userName = "tommy";
      sessionPath = [
        "$HOME/bin"
        "$HOME/.local/bin"
      ];
      sessionVariables = rec {
        EDITOR = "nvim";
        VISUAL = EDITOR;
        BROWSER = "zen-browser";
      };
      colors = gruvbox-material;
      gruvbox-material = {
        primary = {
          background = "1d2021";
          foreground = "d4be98";
          accent = "d8a657";
        };
        normal = {
          black = "32302f";
          red = "ea6962";
          green = "a9b665";
          yellow = "d8a657";
          blue = "7daea3";
          magenta = "d3869b";
          cyan = "89b482";
          white = "d4be98";
        };
        bright = {
          black = "32302f";
          red = "ea6962";
          green = "a9b665";
          yellow = "d8a657";
          blue = "7daea3";
          magenta = "d3869b";
          cyan = "89b482";
          white = "d4be98";
        };
        dim = {
          black = "32302f";
          red = "ea6962";
          green = "a9b665";
          yellow = "d8a657";
          blue = "7daea3";
          magenta = "d3869b";
          cyan = "89b482";
          white = "d4be98";
        };
      };
    };

    forAllSystems = function:
      nixpkgs.lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ] (system: function nixpkgs.legacyPackages.${system});

    mkSystem = hostname: system: let
      systemFunc =
        if (nixpkgs.lib.hasSuffix "linux" system)
        then nixpkgs.lib.nixosSystem
        else darwin.lib.darwinSystem;

      hmModules =
        if (nixpkgs.lib.hasSuffix "linux" system)
        then home-manager.nixosModules
        else home-manager.darwinModules;

      pkgs-stable = import nixpkgs-stable {
        inherit system;
        config.allowUnfree = true;
      };
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };

      specialArgs = {inherit settings pkgs-stable;};
    in {
      "${hostname}" = systemFunc {
        inherit system;
        inherit pkgs;
        inherit specialArgs;
        modules = [
          (./hosts/. + "/${hostname}/" + ./configuration.nix)
          ./modules/system

          hmModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = specialArgs;
              users."${settings.userName}".imports = [./modules/hm (./hosts/. + "${hostname}" + ./home.nix)];
            };
          }
        ];
      };
    };
  in {
    # NOTE:
    # rebuild commands should be
    # - sudo nixos-rebuild switch --flake "$HOME/.dotfiles"
    # - darwin-rebuild switch --flake "$HOME/.dotfiles"
    nixosConfigurations = nixpkgs.lib.mkMerge [
      (mkSystem "kain" "x86_64-linux")
      (mkSystem "raziel" "x86_64-linux")
      (mkSystem "vorador" "aarch64-linux")
    ];
    darwinConfigurations = nixpkgs.lib.mkMerge [
      (mkSystem "tommysmbp" "aarch64-darwin")
    ];

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
