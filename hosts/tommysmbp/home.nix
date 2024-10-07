{lib, ...}: {
  home.homeDirectory = lib.mkForce "/Users/tommy";
  programs = {
    home-manager.enable = true;
    bash.enable = true;
  };

  homeConf = {
    code.enable = true;
    desktop.enable = true;
    dm.enable = true;
  };
}
