{lib, ...}: {
  options = {
    homeConf.linux.enable = lib.mkEnableOption "Enable hm-linux role";
  };
}
