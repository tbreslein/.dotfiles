{lib, ...}: {
  options = {
    homeConf.laptop.enable = lib.mkEnableOption "Enable hm-laptop role";
  };
}
