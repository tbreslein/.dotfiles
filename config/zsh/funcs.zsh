tm() {
    if [ "$TMUX" != "" ]; then
        if ! tmux has-session -t home; then
            tmux new-session -ds "home" -c "$HOME"
        fi
    else
        tmux new-session -ds "home" -c "$HOME"
        tmux a -t "home"
    fi
}

toggle_moco() {
    if ! tmux has-session -t "moco" 2>/dev/null; then
        tmux new-session -ds "moco" -c "$HOME/work/repos/mocotrackingclient/"
        tmux send-keys -t "moco" "poetry install; AUTO_STOP_AND_NAG=False poetry run python moco_client.py" C-m
    else
        tmux kill-session -t "moco"
    fi
}

twork() {
    if [ "$TMUX" != "" ]; then
        pushd "$HOME/work" || exit
        toggle_moco
        popd || exit
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
    if echo "$my_branch" | grep -q "remotes/origin"; then
        my_branch=''${my_branch##remotes/origin/}
    fi
    if echo "$my_branch" | grep -q -P --regexp='\*'; then
        my_branch=''${my_branch##\*}
    fi

    git checkout "$my_branch"
}

ft() {
    selected_task=$(task --list | grep '^\\*' | fzf | cut -d" " -f2 | awk '{ print substr($0,1,length($0)-1) }')
    task "$selected_task"
}

vterm_printf() {
    if [ -n "$TMUX" ] \
        && { [ "${TERM%%-*}" = "tmux" ] \
            || [ "${TERM%%-*}" = "screen" ]; }; then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
