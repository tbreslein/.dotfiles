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
    home.packages = with pkgs; [(nerdfonts.override {fonts = ["Hack"];})];
    nix = {
      package = pkgs.nix;
      settings.extra-experimental-features = ["nix-command" "flakes"];
    };
    fonts.fontconfig.enable = true;
  };
}
# fonts.packages = with pkgs; [
#   (nerdfonts.override
#     {fonts = ["Hack"];})
# ];
