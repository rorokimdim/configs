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

#
# tmux
#
if [ -z "$TMUX" ]
then
    tmux attach -t local || tmux new -s local
fi

#
# Autocompletion
#
zstyle ':completion:*:*:git:*' script ~/.git-completion.zsh
fpath=(~/.zsh $fpath)
autoload -Uz compinit && compinit

if [ -f ~/.shellrc ]; then
    source ~/.shellrc
fi
