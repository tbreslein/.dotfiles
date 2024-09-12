{lib, ...}: {
  options = {
    sysConf.linux.enable = lib.mkEnableOption "Enable sys-linux role";
  };
}
