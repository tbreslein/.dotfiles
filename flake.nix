{
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
  }: {
    # homeConfigurations."tommy" =
    #   home-manager.lib.homeManagerConfiguration {
    #   };

    darwinConfigurations."tommysmbp" = let
      system = "aarch64-darwin";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
    in
      nix-darwin.lib.darwinSystem {
        inherit system;
        inherit pkgs;
        modules = [
          {
            system.stateVersion = 5;
            services.nix-daemon.enable = true;
          }

          home-manager.darwinModules.home-manager
          {
            home-manager = {
              # users.users."tommy".home.stateVersion = 5;
            };
          }
        ];
      };
  };
}
# {
#   description = "my nixos and nix-darwin based dotfiles";
#
#   inputs = {
#     nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
#     nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.05";
#     nix-darwin.url = "github:lnl7/nix-darwin";
#     nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
#     nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
#     home-manager.url = "github:nix-community/home-manager";
#     home-manager.inputs.nixpkgs.follows = "nixpkgs";
#   };
#
#   outputs = {
#     nixpkgs,
#     nixpkgs-stable,
#     home-manager,
#     nix-darwin,
#     nix-homebrew,
#     ...
#   }: let
#     settings = rec {
#       sessionPath = [
#         "$HOME/bin"
#         "$HOME/.local/bin"
#       ];
#       sessionVariables = rec {
#         EDITOR = "nvim";
#         VISUAL = EDITOR;
#         BROWSER = "brave";
#       };
#       fonts = ["MartianMono" "GeistMono" "Iosevka" "Hack"];
#       colors = melange;
#       melange = rec {
#         primary = {
#           background = "292522";
#           foreground = "ece1d7";
#         };
#         normal = {
#           black = "34302c";
#           red = "bd8183";
#           green = "78997a";
#           yellow = "e49b5d";
#           blue = "7f91b2";
#           magenta = "b380b0";
#           cyan = "7b9695";
#           white = "c1a78e";
#         };
#         bright = {
#           black = "868462";
#           red = "d47766";
#           green = "85b695";
#           yellow = "ebc06d";
#           blue = "a3a9ce";
#           magenta = "cf9bc2";
#           cyan = "89b3b6";
#           white = "ece1d7";
#         };
#         dim = normal;
#       };
#       kanagawa-dragon = rec {
#         primary = {
#           background = "181616";
#           foreground = "c5c9c5";
#         };
#         indexed = ["b6927b" "b98d7b"];
#         normal = {
#           black = "0d0c0c";
#           red = "c4746e";
#           green = "8a9a7b";
#           yellow = "c4b28a";
#           blue = "8ba4b0";
#           magenta = "a292a3";
#           cyan = "8ea4a2";
#           white = "c8c093";
#         };
#         bright = {
#           black = "a6a69c";
#           red = "e46876";
#           green = "87a987";
#           yellow = "e6c384";
#           blue = "7fb4ca";
#           magenta = "938aa9";
#           cyan = "7aa89f";
#           white = "c5c9c5";
#         };
#         dim = normal;
#       };
#       gruvbox-material = {
#         primary = {
#           background = "282828";
#           foreground = "d4be98";
#         };
#         indexed = ["d8a657"];
#         normal = {
#           black = "32302f";
#           red = "ea6962";
#           green = "a9b665";
#           yellow = "d8a657";
#           blue = "7daea3";
#           magenta = "d3869b";
#           cyan = "89b482";
#           white = "d4be98";
#         };
#         bright = {
#           black = "32302f";
#           red = "ea6962";
#           green = "a9b665";
#           yellow = "d8a657";
#           blue = "7daea3";
#           magenta = "d3869b";
#           cyan = "89b482";
#           white = "d4be98";
#         };
#         dim = {
#           black = "32302f";
#           red = "ea6962";
#           green = "a9b665";
#           yellow = "d8a657";
#           blue = "7daea3";
#           magenta = "d3869b";
#           cyan = "89b482";
#           white = "d4be98";
#         };
#       };
#     };
#
#     forAllSystems = function:
#       nixpkgs.lib.genAttrs [
#         "x86_64-linux"
#         "aarch64-linux"
#         "aarch64-darwin"
#       ] (system: function nixpkgs.legacyPackages.${system});
#   in {
#     homeConfigurations."tommy" = let
#       system = "x86_64-linux";
#       pkgs = import nixpkgs {
#         inherit system;
#         config.allowUnfree = true;
#       };
#       pkgs-stable = import nixpkgs-stable {
#         inherit system;
#         config.allowUnfree = true;
#       };
#       extraSpecialArgs = {inherit settings pkgs-stable;};
#     in
#       home-manager.lib.homeManagerConfiguration {
#         inherit pkgs;
#         inherit extraSpecialArgs;
#         modules = [
#           ./hosts/kain/home.nix
#           ./modules
#         ];
#       };
#
#     darwinConfigurations."tommysmbp" = let
#       system = "aarch64-darwin";
#       pkgs = import nixpkgs {
#         inherit system;
#         config.allowUnfree = true;
#       };
#       pkgs-stable = import nixpkgs-stable {
#         inherit system;
#         config.allowUnfree = true;
#       };
#       specialArgs = {inherit settings pkgs-stable;};
#     in
#       nix-darwin.lib.darwinSystem {
#         inherit system;
#         inherit pkgs;
#         inherit specialArgs;
#         modules = [
#           ./hosts/tommysmbp/configuration.nix
#
#           nix-homebrew.darwinModules.nix-homebrew
#           {
#             nix-homebrew = {
#               enable = true;
#               enableRosetta = true;
#               user = "tommy";
#             };
#           }
#
#           home-manager.darwinModules.home-manager
#           {
#             home-manager = {
#               useGlobalPkgs = true;
#               useUserPackages = true;
#               extraSpecialArgs = specialArgs;
#               users."tommy".imports = [./modules ./hosts/tommysmbp/home.nix];
#             };
#           }
#         ];
#       };
#
#     devShells = forAllSystems (pkgs: {
#       default = pkgs.mkShell {
#         packages = with pkgs; [
#           vim
#           luajitPackages.lua-lsp
#           stylua
#         ];
#       };
#     });
#   };
# }
