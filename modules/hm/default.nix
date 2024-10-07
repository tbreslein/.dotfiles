{
  config,
  settings,
  ...
}: {
  imports = [
    ./code
    ./desktop
    ./dm
    ./laptop
    ./linux
    ./raspberry
  ];

  home = {
    username = "tommy";
    stateVersion = "24.05";
    shellAliases = {
      g = "git";
      gg = "git status -s";
      GG = "git status";
      v = "$EDITOR";
      ls = "eza --icons=always";
      la = "ls -aa";
      ll = "ls -l";
      lla = "ls -la";
      lt = "eza --tree";
      cp = "cp -i";
      rm = "rm -i";
      mv = "mv -i";
      mkdir = "mkdir -p";
      m = "make";
      ".." = "cd ..";
      "..." = "cd ../..";
      "...." = "cd ../../..";
      rip_nvim = "rm -fr $HOME/.local/share/nvim/ $HOME/.local/state/nvim $HOME/.cache/nvim";
    };
    inherit (settings) sessionPath;
    inherit (settings) sessionVariables;
  };

  manual = {
    html.enable = true;
    manpages.enable = true;
  };

  programs = {
    home-manager.enable = true;
    bash = {
      enable = true;
      enableCompletion = true;
      bashrcExtra =
        /*
        bash
        */
        ''
          stty -ixon # disable c-s and c-q
          shopt -s autocd

          _err_msg=""
          _git_prompt=""
          _prompt_command() {
            _RET=$?
            _err_msg=""
            [ $_RET -gt 0 ] && _err_msg="''${_RET} "
            _git_prompt=""
            if git rev-parse > /dev/null 2>&1; then
              _git_prompt="| $(git rev-parse --abbrev-ref HEAD) "
              if [ $(git status --porcelain=v1 | wc -l) -gt 0 ]; then
                _git_prompt="''${_git_prompt}!"
              fi
              status_uno=$(git status -uno)
              if echo "$status_uno" | grep -q "Your branch is behind"; then
                _git_prompt="''${_git_prompt}v"
              fi
              if echo "$status_uno" | grep -q "Your branch is ahead"; then
                _git_prompt="''${_git_prompt}^"
              fi
              _git_prompt="''${_git_prompt} "
            fi
          }
          PROMPT_COMMAND=_prompt_command
          PS1="\n[\[\033[0;32m\]\u@\h\[\033[0m\] | \[\033[0;34m\]\W\[\033[0m\]\[\033[0;36m\]\''${_git_prompt}\[\033[0m\]]\n\[\033[1;31m\]\''${_err_msg}\[\033[0m\]$ "

          toggle_moco() {
            if ! tmux has-session -t "moco" 2>/dev/null; then
              tmux new-session -ds "moco" -c "$HOME/work/repos/mocotrackingclient/"
              tmux send-keys -t "moco" "poetry install; poetry run python moco_client.py" C-m
            else
              tmux kill-session -t "moco"
            fi
          }

          twork() {
            if [ -n "$TMUX" ]; then
              pushd "$HOME/work"
              toggle_moco
              popd
              if ! tmux has-session -t work; then
                tmux new-session -ds "work" -c "$HOME/work"
              fi
            else
              tmux new-session -ds "work" -c "$HOME/work/"
              tmux send-keys -t "work" "toggle_moco" C-m
              tmux a -t "work"
            fi
          }

          gco() {
              my_branch=$(git branch -a --no-color | sort | uniq | tr -d " " | fzf --select-1 --ansi --preview 'git log --graph --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" {} 2>/dev/null')
              if echo $my_branch | grep -q "remotes/origin"; then
                  my_branch=''${my_branch##remotes/origin/}
              fi
              if echo "$my_branch" | grep -q -P --regexp='\*'; then
                  my_branch=''${my_branch##\*}
              fi

              git checkout $my_branch
          }

          ft() {
            selected_task=$(task --list | grep '^\\*' | fzf | cut -d" " -f2 | awk '{ print substr($0,1,length($0)-1) }')
            task "$selected_task"
          }
        '';
      initExtra = "";
      profileExtra =
        /*
        bash
        */
        ''
          [ -d "$HOME/.cargo" ] && [ -f "$HOME/.cargo/env" ] && source "$HOME/.cargo/env"
        '';
    };
    readline = {
      enable = true;
      extraConfig =
        /*
        bash
        */
        ''
          set colored-stats on
          set colored-completion-prefix on
          set completion-ignore-case on
          set editing-mode vi
        '';
    };
    bat.enable = true;
    direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv.enable = true;
      silent = true;
    };
    eza = {
      enable = true;
      enableBashIntegration = true;
    };
    fd.enable = true;
    fzf = {
      enable = true;
      enableBashIntegration = true;
    };
    git = {
      enable = true;
      delta = {
        enable = true;
        options = {
          line-numbers = true;
          true-color = "always";
        };
      };
      extraConfig = {
        core = {
          editor = "nvim -c 'startinsert'";
        };
        pull.rebase = true;
        push = {
          autoSetupRemote = true;
          default = "simple";
        };
        rerere.enabled = true;
        user = {
          name = "Tommy Breslein";
          email = "tommy.breslein@protonmail.com";
        };
      };
      includes = [
        {
          condition = "gitdir:${config.home.homeDirectory}/work/";
          contents.user.email = "tommy.breslein@pailot.com";
        }
      ];
    };
    htop.enable = true;
    less.enable = true;
    man.enable = true;
    ripgrep.enable = true;
    tealdeer = {
      enable = true;
      settings.updates.auto_update = true;
    };
    zoxide = {
      enable = true;
      enableBashIntegration = true;
    };
  };
}
