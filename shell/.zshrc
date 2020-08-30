if [ -f ~/.shellrc ]; then
    source ~/.shellrc
fi

zstyle ':completion:*:*:git:*' script ~/.git-completion.zsh
fpath=(~/.zsh $fpath)

autoload -Uz compinit && compinit
