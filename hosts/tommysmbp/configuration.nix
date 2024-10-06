{pkgs, ...}: {
  environment = {
    shells = [pkgs.bashInteractive pkgs.zsh];
  };
  fonts.packages = with pkgs; [
    (nerdfonts.override
      {fonts = ["Hack" "Gohu"];})
  ];
  nix = {
    package = pkgs.nix;
    settings."extra-experimental-features" = ["nix-command" "flakes"];
  };
  programs = {
    bash = {
      enable = true;
      enableCompletion = true;
    };
  };
  services.nix-daemon.enable = true;
  system.stateVersion = 5;
  users.users."tommy" = {
    shell = pkgs.bashInteractive;
  };
}
