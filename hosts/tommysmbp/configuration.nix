{ pkgs, ... }: {
  environement = {
    shells = [ pkgs.bashInteractive pkgs.zsh ];
  };
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

  #system.stateVersion = "24.05";
  system.stateVersion = 5;

  users.users."tommy" = {
    shell = pkgs.bashInteractive;
  };

  programs = {
    bash = {
      enable = true;
      enableCompletion = true;
    };
  };
}
