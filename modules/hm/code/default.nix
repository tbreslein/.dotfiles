{lib, ...}: {
  options = {
    homeConf.code.enable = lib.mkEnableOption "Enable hm-code role";
  };
}
