if status is-interactive
    alias rm="rm -i"
    alias cp="cp -i"
    alias mv="mv -i"
    alias mkdir="mkdir -p"
    alias m="make"
    alias v="nvim"
    alias g="git"
    alias lg="lazygit"
    alias gg="git status -s"
    alias GG="git status"
    alias ls="eza --icons=always"
    alias la="ls -aa"
    alias ll="ls -l"
    alias lla="ls -la"
    alias lt="eza --tree"
    alias rip_nvim="rm -fr $HOME/.local/share/nvim/ $HOME/.local/state/nvim/ $HOME/.cache/nvim/"
end

set -U fish_greeting
fish_vi_key_bindings

set -gx EDITOR nvim
set -gx BROWSER brave
set -gx DOTFILES "$HOME/.dotfiles"
set -gx MYCONFIG "$DOTFILES/config"
set -gx DOTCONFIG "$HOME/.config"
set -gx HOMEBREW_BUNDLE_FILE "$DOTFILES/pkgs/Brewfile"

set dm_cache "$HOME/.local/cache/dm"

fish_add_path ~/.local/bin
fish_add_path ~/.cargo/bin

function toggle_moco
    if not tmux has-session -t moco 2>/dev/null
        tmux new-session -ds moco -c "$HOME/work/repos/mocotrackingclient/"
        tmux send-keys -t moco "poetry install; AUTO_STOP_AND_NAG=False poetry run python moco_client.py" C-m
    else
        tmux kill-session -t moco
    end
end

function twork
    if test -n "$TMUX"
        pushd "$HOME/work/"
        toggle_moco
        popd
        if not tmux has-session -t work
            tmux new-session -ds work -c "$HOME/work"
        end
    else
        tmux new-session -ds work -c "$HOME/work"
        tmux send-keys -t work toggle_moco C-m
        tmux a -t work
    end
end

#function gco
#    set my_branch "$(git branch -a --no-color | sort | uniq | tr -d ' ' | fzf --select-1 --ansi --preview 'git log --graph --color--always --format=\"%C(auto)%h%d %s %C(black)%C(bold)%cr\" {} 2>/dev/null')"
#    if $(echo $my_branch | grep -q "remotes/origin")
#        set my_branch $my_branch[15..]
#    end
#    if $(echo $my_branch | grep -q -P --regexp='\*')
#        set my_branch $my_branch[1..]
#    end
#    echo $my_branch
#    # git checkout $my_branch
#end

if test -d /opt/homebrew
    /opt/homebrew/bin/brew shellenv | source
end
if test -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish'
    source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish'
end

if command -v fzf &>/dev/null
    fzf --fish | source
end
if command -v zoxide &>/dev/null
    zoxide init fish | source
end

function starship_transient_prompt_func
    starship module character
end
if command -v starship &>/dev/null
    starship init fish | source
    enable_transience
end
if command -v direnv &>/dev/null
    direnv hook fish | source
end
