{ ... }: {
  imports = [
    ./code
    ./desktop
    ./dm
    ./laptop
    ./linux
    ./raspberry
  ];

  # add common home manager configs here
  programs = {
    home-manager.enable = true;
    direnv = { enable = true; enableBashIntegration = true; nix-direnv.enable = true; silent = true; };
  };
}
