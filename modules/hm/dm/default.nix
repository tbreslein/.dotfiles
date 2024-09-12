{lib, ...}: {
  options = {
    homeConf.dm.enable = lib.mkEnableOption "Enable dm role";
  };
}
