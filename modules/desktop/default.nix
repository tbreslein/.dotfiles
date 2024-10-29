{
  config,
  lib,
  pkgs,
  settings,
  ...
}: let
  cfg = config.homeConf.desktop;
in {
  options = {
    homeConf.desktop = {
      enable = lib.mkEnableOption "Enable hm-desktop role";
      terminalFontSize = lib.mkOption {
        default = 18;
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
            opacity = 0.94;
            blur = true;
            startup_mode = "Maximized";
            option_as_alt = "OnlyLeft";
          };
          font = {
            size = cfg.terminalFontSize;
            normal.family = (builtins.elemAt settings.fonts 0) + " Nerd Font";
          };
          colors = {
            primary = {
              background = "#${settings.colors.primary.background}";
              foreground = "#${settings.colors.primary.foreground}";
            };
            normal = {
              black = "#${settings.colors.normal.black}";
              red = "#${settings.colors.normal.red}";
              green = "#${settings.colors.normal.green}";
              yellow = "#${settings.colors.normal.yellow}";
              blue = "#${settings.colors.normal.blue}";
              magenta = "#${settings.colors.normal.magenta}";
              cyan = "#${settings.colors.normal.cyan}";
              white = "#${settings.colors.normal.white}";
            };
            bright = {
              black = "#${settings.colors.bright.black}";
              red = "#${settings.colors.bright.red}";
              green = "#${settings.colors.bright.green}";
              yellow = "#${settings.colors.bright.yellow}";
              blue = "#${settings.colors.bright.blue}";
              magenta = "#${settings.colors.bright.magenta}";
              cyan = "#${settings.colors.bright.cyan}";
              white = "#${settings.colors.bright.white}";
            };
            dim = {
              black = "#${settings.colors.dim.black}";
              red = "#${settings.colors.dim.red}";
              green = "#${settings.colors.dim.green}";
              yellow = "#${settings.colors.dim.yellow}";
              blue = "#${settings.colors.dim.blue}";
              magenta = "#${settings.colors.dim.magenta}";
              cyan = "#${settings.colors.dim.cyan}";
              white = "#${settings.colors.dim.white}";
            };
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
