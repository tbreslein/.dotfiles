{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.homeConf.code;
  tmux_sessionizer =
    pkgs.writeShellScriptBin "tmux-sessionizer"
    /*
    bash
    */
    ''
      folders=("$HOME")
      add_dir() {
          [ -d "$HOME/$1" ] && folders+=("$HOME/$1")
      }

      add_dir "code"
      add_dir "notes"
      add_dir "work"
      add_dir "work/repos"

      if [[ $# -eq 1 ]]; then
          selected=$1
      else
          selected=$(find $(echo "''${folders[@]}") -mindepth 1 -maxdepth 1 -type d | fzf)
      fi

      if [[ -z $selected ]]; then
          exit 0
      fi

      selected_name=$(basename "$selected" | tr . _)
      tmux_running=$(pgrep tmux)

      if [[ -z $TMUX ]] && [[ -z $tmux_running ]]; then
          tmux new-session -s "$selected_name" -c "$selected"
          exit 0
      fi

      if ! tmux has-session -t="$selected_name" 2>/dev/null; then
          tmux new-session -ds "$selected_name" -c "$selected"
      fi

      tmux switch-client -t "$selected_name"
    '';
in {
  options = {
    homeConf.code.enable = lib.mkEnableOption "Enable hm-code role";
  };

  config = lib.mkIf cfg.enable {
    home = {
      packages = with pkgs; [
        emacs-lsp-booster
        jq
        neovim
        tmux_sessionizer
        marksman
        nodePackages.prettier
        bash-language-server
        nixd
        statix
        alejandra
      ];
      file.".luacheckrc" = {
        text = ''
          globals = { "vim", "LAZY_PLUGIN_SPEC", "spec", "kmap" }
        '';
      };
    };

    editorconfig = {
      enable = true;
      settings = {
        "*" = {
          charset = "utf-8";
          indent_size = 4;
          indent_style = "space";
          max_line_width = 80;
          trim_trailing_whitespace = true;
        };
        "*.{json,js,jsx,ts,tsx,cjs,mjs,nix,cabal,hs,lua,yml,yaml,ml,mli,hl,md,mdx,html,astro}" = {
          indent_size = 2;
        };
        "CMakeLists.txt" = {
          indent_size = 2;
        };
        "{m,M}akefile" = {
          indent_style = "tab";
        };
      };
    };

    programs = {
      tmux = {
        enable = true;
        shell = "${pkgs.bashInteractive}/bin/bash";
        extraConfig =
          /*
          tmux
          */
          ''
              unbind C-b
              set-option -g prefix C-a
              bind-key C-a send-prefix

              set -g default-terminal "alacritty"
              set -sa terminal-overrides ",alacritty:RGB"

              bind-key -r f run-shell "tmux new-window ${tmux_sessionizer}/bin/tmux-sessionizer"

              set -s escape-time 0
            #setw -g mode-keys vi
              set -g status-keys vi
              set -g mouse on
              set -g history-limit 25000
              set -g status-position top
              set -g status-style "fg=colour3 bg=colour0 bold"
              set -g status-left ""
              set -g status-right "Session: #S "

              bind C-s split-window -v -c "#{pane_current_path}"
              bind C-v split-window -hb -c "#{pane_current_path}"
              bind h select-pane -L
              bind j select-pane -D
              bind k select-pane -U
              bind l select-pane -R
              bind -r M-h resize-pane -L 1
              bind -r M-j resize-pane -D 1
              bind -r M-k resize-pane -U 1
              bind -r M-l resize-pane -R 1
              bind C-r source-file ~/.config/tmux/tmux.conf
          '';
      };
    };
  };
}
