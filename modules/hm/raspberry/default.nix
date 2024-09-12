{lib, ...}: {
  options = {
    homeConf.raspberyy.enable = lib.mkEnableOption "Enable hm-raspberry role";
  };
}
