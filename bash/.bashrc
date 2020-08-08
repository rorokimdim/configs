export TERM=screen-256color
export EDITOR=vim

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

if [ -f ~/.custom_bashrc ]; then
    source ~/.custom_bashrc
fi

if [ -f ~/.git-completion.bash ]; then
  source ~/.git-completion.bash
fi
