#!/usr/bin/env bash

mkdir -p "~/.emacs.d"
mkdir -p "~/.config"

apps=(
  stow
  git
  brittany
  closh
  emacs
  ranger
  tmux
  vim
  python
  shell
  vscode
)

for app in ${apps[@]}; do
  stow -v -R -t $HOME $app
done
