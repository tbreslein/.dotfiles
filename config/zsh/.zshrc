export HISTFILE="$HOME/.zsh_history"
export ZSH_COMPDUMP="$HOME/.zcompdump"
export HISTSIZE=1000000
export SAVEHIST=1000000

source $ZDOTDIR/aliases.zsh
source $ZDOTDIR/funcs.zsh

autoload -Uz compinit && compinit

eval "$(direnv hook zsh)"
eval "$(fzf --zsh)"
eval "$(starship init zsh)"
eval "$(zoxide init zsh)"
