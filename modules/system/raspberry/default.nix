{lib, ...}: {
  options = {
    sysConf.raspberry.enable = lib.mkEnableOption "Enable sys-raspberry role";
  };
}
