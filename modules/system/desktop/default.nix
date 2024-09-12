{lib, ...}: {
  imports = [./gaming];
  options = {
    sysConf.desktop.enable = lib.mkEnableOption "Enable sys-desktop role";
  };
}
