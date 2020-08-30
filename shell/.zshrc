if [ -f ~/.shellrc ]; then
    source ~/.shellrc
fi

PROMPT='%~ $ '

if [ -f ~/.zplugrc ]; then
    source ~/.zplugrc
fi

#
# Autocompletion
#
zstyle ':completion:*:*:git:*' script ~/.git-completion.zsh
fpath=(~/.zsh $fpath)
autoload -Uz compinit && compinit
