export VIRTUAL_ENV_DISABLE_PROMPT=0
PROMPT='%~ $ '

if [ -f ~/.zplugrc ]; then
    source ~/.zplugrc
fi

#
# Enable Ctrl-x-Ctrl-e/Ctrl-x-e to edit command line
#
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

# Make fg/bg work
set -o monitor

#
# Autocompletion
#
fpath=(~/.zsh $fpath)
autoload -Uz compinit && compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

if [ -f ~/.shellrc ]; then
    source ~/.shellrc
fi
