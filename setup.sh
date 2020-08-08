#!/usr/bin/env bash

apps=(
  stow
  git
  bash
  brittany
  closh
  emacs
  ranger
  tmux
  vim
  vscode
)

for app in ${apps[@]}; do
  stow -v -R -t $HOME $app
done
