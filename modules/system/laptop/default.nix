{lib, ...}: {
  options = {
    sysConf.laptop.enable = lib.mkEnableOption "Enable sys-laptop role";
  };
}
