{lib, ...}: {
  home = {
    username = "tommy";
    homeDirectory = lib.mkForce "/Users/tommy/";
    stateVersion = "24.05";
  };
  programs = {
    home-manager.enable = true;
    bash.enable = true;
  };

  homeConf = {
    code.enable = true;
    desktop.enable = true;
  };
}
