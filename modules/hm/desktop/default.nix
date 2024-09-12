{lib, ...}: {
  options = {
    homeConf.desktop.enable = lib.mkEnableOption "Enable hm-desktop role";
  };
}
