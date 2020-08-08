alias ls="ls -GFh"
alias rm='rm -i'
alias mv='mv -i'
alias cp='cp -i'
alias e='emacsclient -t --alternate-editor ""'
alias vim='emacsclient -t --alternate-editor ""'

alias v="/usr/local/bin/nvim"
alias o='emacsclient -t --alternate-editor "" $(fzf)'
alias r='ranger'
alias j='cd -P ~/workspace/$(ls -d ~/workspace/*/ | sed "s/.*workspace//" | sed "s:/::g" | fzf)'
alias tmux='tmux -u'

alias httpserver='python3 -m http.server'

if [ -f ~/.custom_bash_aliases ]; then
    . ~/.custom_bash_aliases
fi
