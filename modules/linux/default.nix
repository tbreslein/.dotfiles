{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.homeConf.code;
in {
  options = {
    homeConf.linux.enable = lib.mkEnableOption "Enable hm-linux role";
  };

  config = lib.mkIf cfg.enable {
    home = {
      packages = with pkgs; [(nerdfonts.override {fonts = ["Hack"];})];
      file = {
        ".config/electron/electron-flags.conf".text = ''
          --ozone-platform-hint=auto
          --ozone-platform=wayland
          --enable-webrtc-pipewire-capturer
          --enable-features=WaylandWindowDecorations
          --gtk-version=4
        '';
        ".config/electron-flags.conf".text = ''
          --ozone-platform-hint=auto
          --enable-features=WaylandWindowDecorations
          --ozone-platform=wayland
        '';
        ".config/electron13/electron13-flags.conf".text = ''
          --ozone-platform-hint=auto
          --ozone-platform=wayland
          --enable-features=UseOzonePlatform
          --enable-features=WaylandWindowDecorations
        '';
        ".config/electron13-flags.conf".text = ''
          --ozone-platform-hint=auto
          --enable-features=WaylandWindowDecorations
          --ozone-platform=wayland
        '';
      };
      sessionVariables = {
        QT_QPA_PLATFORM = "wayland";
        MOZ_ENABLE_WAYLAND = 1;
        ELECTRON_OZONE_PLATFORM_HINT = "wayland";
      };
    };
    nix = {
      package = pkgs.nix;
      settings.extra-experimental-features = ["nix-command" "flakes"];
    };
    fonts.fontconfig.enable = true;
  };
}
