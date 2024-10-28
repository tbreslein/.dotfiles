{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.homeConf.dm;
  dm-script =
    pkgs.writeShellScriptBin "dm"
    /*
    bash
    */
    ''
      set -euo pipefail

      readonly COLOR_RESET="\033[0m"
      readonly BRIGHT_RED="\033[1;31m"
      readonly BRIGHT_GREEN="\033[1;32m"
      readonly BRIGHT_YELLOW="\033[1;33m"
      readonly BRIGHT_BLUE="\033[1;34m"
      SUCCESS=0
      SUCCESS_SKIP=1
      SUCCESS_WARN=2
      FAIL=3
      cmd_code=$SUCCESS
      cmd_msg=""
      cmd_location=""

      readonly DOTFILES="$HOME/.dotfiles"

      pushd() {
          command pushd "$@" >/dev/null
      }

      popd() {
          command popd >/dev/null
      }
      __log() {
          name="$1"
          state="$2"
          color="$3"
          msg="$4"
          echo -e "$color[ dm:$name ] $state |$COLOR_RESET $msg"
      }

      __info() {
          if [ $# -gt 0 ]; then
              name="$1"
              msg="$2"
          else
              name="$cmd_location"
              msg="$cmd_msg"
          fi
          __log "$name" "INFO" "$BRIGHT_BLUE" "$msg"
      }

      __warn() {
          if [ $# -gt 0 ]; then
              name="$1"
              msg="$2"
          else
              name="$cmd_location"
              msg="$cmd_msg"
          fi
          name="$cmd_location"
          msg="$cmd_msg"
          __log "$name" "WARN" "$BRIGHT_YELLOW" "$msg"
      }

      __fail() {
          if [ $# -gt 0 ]; then
              name="$1"
              msg="$2"
          else
              name="$cmd_location"
              msg="$cmd_msg"
          fi
          name="$cmd_location"
          msg="$cmd_msg"
          __log "$name" "FAIL" "$BRIGHT_RED" "$msg"
      }

      __success() {
          if [ $# -gt 0 ]; then
              name="$1"
              msg="$2"
          else
              name="$cmd_location"
              msg="$cmd_msg"
          fi
          __log "$name" "SUCCESS" "$BRIGHT_GREEN" "$msg"
      }

      __handle_err() {
          case "$cmd_code" in
              "$SUCCESS_SKIP") __success ;;
              "$SUCCESS_WARN") __warn ;;
              "$FAIL")
                  __fail
                  exit "$cmd_code"
                  ;;
          esac

          cmd_code=$SUCCESS
          cmd_msg=""
          cmd_location=""
      }

      dm_repos() {
          name="repos"
          __info "$name" "starting"
          for r in ${lib.strings.concatStringsSep " " cfg.repos}; do
              repo_name=$(echo "$r" | sed -E "s/.*tbreslein\/(.*).git$/\1/g")
              repo_dir="$HOME/code/$repo_name"
              if [ -d "$repo_dir" ]; then
                  pushd "$repo_dir"
                  git pull &
                  popd
              else
                  git clone "$r" "$repo_dir" &
              fi
          done
          wait

          __success "$name" "finished"
      }

      dm_hm() {
          name="hm"
          __info "$name" "starting"

          pushd "$DOTFILES"
          case $(uname -s) in
              "Linux") home-manager switch --extra-experimental-features "nix-command flakes" --flake .;;
              "Darwin") darwin-rebuild switch --flake .;;
              *);;
          esac

          __success "$name" "finished"
          popd
      }

      dm_nix() {
          name="nix"
          __info "$name" "starting"

          pushd "$DOTFILES"
          nix-store --gc
          nix-store --optimise
          nix flake update

          __success "$name" "finished"
          popd
      }

      dm_pkgs() {
          name="pkgs"
          __info "$name" "starting"

          if [[ $(uname -s) == "Linux" ]]; then
            sudo pacman -Syu
            paru
          fi
          if command -v rustup &>/dev/null; then
              __info "$name" "running rustup"
              rustup update
          fi

          __success "$name" "finished"
      }

      dm_nvim() {
        name="nvim"
        __info "$name" "starting"
        if command -v nvim &>/dev/null; then
          nvim --headless "+Lazy! sync" "+TSUpdateSync" +qa
        fi
        __success "$name" "finished"
      }

      dm_sync() {
          __info "sync" "starting"

          IFS=' ' read -ra __commands <<<"$VALID_COMMANDS"
          for c in "''${__commands[@]}"; do
              [[ $c == "sync" ]] && continue
              eval "dm_$c"
              __handle_err
          done

          __success "sync" "finished"
      }

      __main() {
          pushd "$DOTFILES"
          git pull || true
          popd
          for c in "''${COMMANDS[@]}"; do
              if [[ ! "$VALID_COMMANDS" =~ [[:space:]]$c[[:space:]] ]]; then
                  cmd_code=$FAIL
                  cmd_location="main"
                  cmd_msg="$c is not a valid dm command; valid commands:$VALID_COMMANDS"
              fi
          done
          __handle_err
          mkdir -p ~/.config ~/.local/bin ~/.cache/dm
          for c in "''${COMMANDS[@]}"; do
              eval "dm_$c"
              __handle_err
          done
          __handle_err
      }

      COMMANDS=()
      VALID_COMMANDS=" repos nix hm pkgs nvim sync "
      if [ $# -gt 0 ]; then
          read -ra COMMANDS <<<"$@"
      else
          COMMANDS=("sync")
      fi
      __main
    '';
in {
  options = {
    homeConf.dm = {
      enable = lib.mkEnableOption "Enable dm role";
      repos = lib.mkOption {
        description = "Which git repos to clone";
        type = with lib.types; listOf str;
        default = [];
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [dm-script];
  };
}
