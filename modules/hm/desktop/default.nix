{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.homeConf.desktop;
in {
  options = {
    homeConf.desktop = {
      enable = lib.mkEnableOption "Enable hm-desktop role";
      terminalFontSize = lib.mkOption {
        default = 24;
        type = lib.types.int;
      };
    };
  };

  config = lib.mkIf cfg.enable {
    programs = {
      alacritty = {
        enable = true;
        package = pkgs.emptyDirectory;
        settings = {
          window = {
            opacity = 0.9;
            blur = true;
            startup_mode = "Maximized";
            option_as_alt = "OnlyLeft";
          };
          font = {
            size = cfg.terminalFontSize;
            # normal.family = "Hack Nerd Font";
            normal.family = "ProggyClean Nerd Font";
          };
        };
      };
      yazi = {
        enable = true;
        enableBashIntegration = true;
      };
    };
  };
}
