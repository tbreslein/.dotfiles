{lib, ...}: {
  options = {
    sysConf.gaming.enable = lib.mkEnableOption "Enable sys-gaming role";
  };
}
