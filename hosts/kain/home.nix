{lib, ...}: {
  home.homeDirectory = lib.mkForce "/home/tommy";
  homeConf = {
    code.enable = true;
    desktop = {
      enable = true;
      terminalFontSize = 12;
    };
    dm.enable = true;
  };
}
