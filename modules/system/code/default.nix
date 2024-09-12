{lib, ...}: {
  options = {
    sysConf.code.enable = lib.mkEnableOption "Enable sys-code role";
  };
}
