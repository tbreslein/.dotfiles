{ pkgs, lib, ... }: {
  home.username = "tommy";
  home.homeDirectory = lib.mkForce "/Users/tommy/";
  home.stateVersion = "24.05";
  programs.home-manager.enable = true;
  programs.bash.enable = true;

  homeConf = {
    code.enable = true;
  };
}
