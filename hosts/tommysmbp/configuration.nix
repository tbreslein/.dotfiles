{pkgs, ...}: {
  homebrew = {
    enable = true;
    brews = [
      "wget"
      "emacs"
      # "coreutils"
      {
        name = "syncthing";
        restart_service = true;
      }

      # "bear"
      # "clang-format"
      # "cppcheck"
      # "gcc"
      # "make"
      # "meson"
      # "ninja"
    ];
    casks = [
      "alacritty"
      "amethyst"
      "rectangle"
    ];
    onActivation = {
      cleanup = "zap";
      autoUpdate = true;
      # upgrade = true;
    };
  };
  environment.shells = [pkgs.bashInteractive pkgs.zsh];
  fonts.packages = with pkgs; [
    (nerdfonts.override
      {fonts = ["Hack" "ProggyClean"];})
  ];
  nix = {
    package = pkgs.nix;
    settings."extra-experimental-features" = ["nix-command" "flakes"];
  };
  programs.bash = {
    enable = true;
    enableCompletion = true;
  };
  services.nix-daemon.enable = true;
  system.stateVersion = 5;
  users.users."tommy".shell = pkgs.bashInteractive;
}
