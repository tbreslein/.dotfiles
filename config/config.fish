if status is-interactive
    # Commands to run in interactive sessions can go here
    alias rm="rm -i"
    alias cp="cp -i"
    alias mv="mv -i"
    alias mkdir="mkdir -p"
    alias m="make"
    alias v="nvim"
    alias g="git"
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

fish_add_path ~/.local/bin

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

set valid_dm_commands ln pkgs nvim sync

function dm_ln
    set -l links "$MYCONFIG/nvim:$DOTCONFIG/nvim"
    set -a links "$MYCONFIG/alacritty/alacritty.toml:$DOTCONFIG/alacritty/alacritty.toml"
    set -a links "$MYCONFIG/alacritty/melange.toml:$DOTCONFIG/alacritty/colors.toml"
    set -a links "$MYCONFIG/config.fish:$DOTCONFIG/fish/config.fish"
    set -a links "$MYCONFIG/editorconfig:$HOME/.editorconfig"
    set -a links "$MYCONFIG/nix.conf:$DOTCONFIG/nix/nix.conf"
    set -a links "$MYCONFIG/tmux.conf:$DOTCONFIG/tmux/tmux.conf"
    set -a links "$MYCONFIG/starship.toml:$DOTCONFIG/starship.toml"
    set -a links "$MYCONFIG/direnv.toml:$DOTCONFIG/direnv/direnv.toml"
    set -a links "$DOTFILES/scripts/tmux_sessionizer:$HOME/.local/bin/tmux_sessionizer"

    switch (uname)
        case Darwin
            set -a links "$MYCONFIG/alacritty/darwin.toml:$DOTCONFIG/alacritty/host.toml"
        case Linux
            switch ($HOSTNAME)
                case kain
                    set -a links "$MYCONFIG/alacritty/kain:$DOTCONFIG/alacritty/host.toml"
            end
    end

    for l in $links
        set -l foo (string split : $l)
        set -l _src $foo[1]
        set -l _target $foo[2]
        if not test -L $_target
            echo "INFO: creating link $foo"
            mkdir -p (path dirname $_target)
            ln -s $_src $_target
        end
    end
end

function dm_pkgs
    switch (uname)
        case Darwin
            if not brew bundle check
                brew bundle --no-lock
            end
            brew bundle cleanup --force
        case Linux
            paru
    end
    if command -v rustup &>/dev/null
        rustup update
    end
end

function dm_nvim
    if command -v nvim &>/dev/null
        nvim --headless "+Lazy! sync" "+TSUpdateSync" +qa
    end
end

function dm_sync
    for c in $valid_dm_commands
        if test $c = sync
            continue
        end
        eval "dm_$c"
    end
end

function dm
    if test (count $argv) -ge 1
        if contains sync $argv
            dm_sync
        else
            for a in $argv
                if not contains $a $valid_dm_commands
                    echo "ERROR: $a is not a valid dm command"
                    return 1
                end
                eval "dm_$a"
            end
        end
    else
        dm_sync
    end
end

if test -d /opt/homebrew
    /opt/homebrew/bin/brew shellenv | source
end
if test -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish'
    source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish'
end

fzf --fish | source
zoxide init fish | source

function starship_transient_prompt_func
    starship module character
end
starship init fish | source
enable_transience
