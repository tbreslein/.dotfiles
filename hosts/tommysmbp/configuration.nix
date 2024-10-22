{
  config,
  pkgs,
  ...
}: {
  homebrew = {
    enable = true;
    brews = [
      "coreutils"
      {
        name = "syncthing";
        restart_service = true;
      }

      # "bear"
      # "clang-format"
      # "cppcheck"
      # "gcc"
      # "libtool"
      # "cmake"
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
  environment = {
    shells = [pkgs.bashInteractive pkgs.zsh];
    # systemPackages = [pkgs.emacs30];
  };
  fonts.packages = with pkgs; [
    (nerdfonts.override
      {fonts = ["Hack"];})
  ];
  nix = {
    package = pkgs.nix;
    settings."extra-experimental-features" = ["nix-command" "flakes"];
  };
  programs.bash = {
    enable = true;
    enableCompletion = true;
  };
  services = {
    # emacs = {
    #   enable = true;
    #   package = pkgs.emacs30;
    # };
    nix-daemon.enable = true;
  };
  system = {
    stateVersion = 5;
    activationScripts.applications.text = let
      env = pkgs.buildEnv {
        name = "system-applications";
        paths = config.environment.systemPackages;
        pathsToLink = "/Applications";
      };
    in
      pkgs.lib.mkForce ''
        # Set up applications.
        echo "setting up /Applications..." >&2
        rm -rf /Applications/Nix\ Apps
        mkdir -p /Applications/Nix\ Apps
        find ${env}/Applications -maxdepth 1 -type l -exec readlink '{}' + |
        while read src; do
          app_name=$(basename "$src")
          echo "copying $src" >&2
          ${pkgs.mkalias}/bin/mkalias "$src" "/Applications/Nix Apps/$app_name"
        done
      '';
  };
  users.users."tommy".shell = pkgs.bashInteractive;
}
