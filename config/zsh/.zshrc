export HISTFILE="$HOME/.zsh_history"
export ZSH_COMPDUMP="$HOME/.zcompdump"
export ZCOMPCACHE="$HOME/.local/zsh/zcompcache"
export HISTSIZE=1000000
export SAVEHIST=1000000

source $ZDOTDIR/aliases.zsh
source $ZDOTDIR/funcs.zsh

autoload -Uz compinit && compinit -d "$ZSH_COMPDUMP"

eval "$(direnv hook zsh)"
eval "$(fzf --zsh)"
eval "$(starship init zsh)"
eval "$(zoxide init zsh)"
