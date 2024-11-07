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

set dm_cache "$HOME/.local/cache/dm"

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

# all this dm nonsense is hopefully soon to be replaced by syke
set valid_dm_commands ln pkgs nvim sync

function dm_ln
    set -l links "$MYCONFIG/nvim:$DOTCONFIG/nvim"
    set -a links "$MYCONFIG/alacritty/alacritty.toml:$DOTCONFIG/alacritty/alacritty.toml"
    set -a links "$MYCONFIG/alacritty/gruvbox-material.toml:$DOTCONFIG/alacritty/colors.toml"
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
            set -a links "$MYCONFIG/aerospace.toml:$DOTCONFIG/aerospace/aerospace.toml"
        case Linux
            switch (cat /etc/hostname)
                case kain
                    set -a links "$MYCONFIG/alacritty/kain:$DOTCONFIG/alacritty/host.toml"
                    set -a links "$MYCONFIG/hypr:$DOTCONFIG/hypr"
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
        else if not test (readlink $_target) = $_src
            echo "INFO: replacing link source for $_target to $_src"
            ln -sf $_src $_target
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
            set -l files
            switch (cat /etc/hostname)
                case kain
                    set -l arch_files \
                        "$DOTFILES/pkgs/archfile-base" \
                        "$DOTFILES/pkgs/archfile-gaming" \
                        "$DOTFILES/pkgs/archfile-kain" \
                        "$DOTFILES/pkgs/archfile-desktop"
                    set -l aur_files \
                        "$DOTFILES/pkgs/aurfile-gaming" \
                        "$DOTFILES/pkgs/aurfile-desktop"

                    _handle_pkgs arch (cat $arch_files | sort --unique)
                    _handle_pkgs aur (cat $aur_files  | sort --unique)

                    set -l pacman_install "$dm_cache/install_arch"
                    set -l pacman_remove "$dm_cache/remove_arch"
                    if test -s "$pacman_remove"
                        sudo pacman -R (awk '{print $1}' "$pacman_remove")
                    end
                    sudo pacman -Syu
                    if test -s "$pacman_install"
                        sudo pacman --needed -S (awk '{print $1}' "$pacman_install")
                    end
                    mv -f "$dm_cache/want_arch" "$dm_cache/have_arch"

                    set -l paru_install "$dm_cache/install_aur"
                    set -l paru_remove "$dm_cache/remove_aur"
                    if test -s "$paru_remove"
                        paru -R (awk '{print $1}' "$paru_remove")
                    end
                    paru --aur -Syu
                    if test -s "$paru_install"
                        paru --aur --needed -S (awk '{print $1}' "$paru_install")
                    end
                    mv -f "$dm_cache/want_aur" "$dm_cache/have_aur"

                    rm -f $dm_cache/install_*
                    rm -f $dm_cache/remove_*
            end
    end
    if command -v rustup &>/dev/null
        rustup update
    end
end

function _handle_pkgs
    set -l pkg_mgr $argv[1]
    set -l want $argv[2..]
    echo $pkg_mgr
    echo $want
    mkdir -p $dm_cache

    set -l file_have "$dm_cache/have_$pkg_mgr"
    touch $file_have

    set -l file_install "$dm_cache/install_$pkg_mgr"
    set -l file_remove "$dm_cache/remove_$pkg_mgr"
    set -l file_want "$dm_cache/want_$pkg_mgr"

    for f in $file_install $file_remove $file_want
        touch $f
        truncate -s0 $f
    end

    for x in $want
        echo $x >>$file_want
    end
    comm -23 $file_want $file_have >$file_install
    comm -13 $file_want $file_have >$file_remove
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
