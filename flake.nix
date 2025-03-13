{
  description = "my dotfiles";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
  };
  outputs = { self, nixpkgs, systems, ... }:
    let
      eachSystem = nixpkgs.lib.genAttrs (import systems);
    in
    {
      devShells = eachSystem
        (system:
          let
            pkgs = import nixpkgs {
              inherit system;
              config.allowUnfree = true;
            };
          in
          {
            default = pkgs.mkShell {
              packages = with pkgs; [
                bash-language-server

                lua-language-server
                stylua
                luajitPackages.luacheck

                nixd
                nixpkgs-fmt
                statix
              ];
            };
          }
        );
    };
}
