{lib, ...}: {
  home.homeDirectory = lib.mkForce "/home/tommy";
  homeConf = {
    code.enable = true;
    linux.enable = true;
    dm.enable = true;
  };
}
