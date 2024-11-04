{lib, ...}: {
  home.homeDirectory = lib.mkForce "/home/tommy";
  homeConf = {
    code.enable = true;
    linux.enable = true;
    desktop = {
      enable = true;
      terminalFontSize = 10;
    };
    dm.enable = true;
  };
}
