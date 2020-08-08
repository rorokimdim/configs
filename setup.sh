#!/usr/bin/env bash

apps=(
  stow
  bash
  brittany
  closh
  emacs
  ranger
  tmux
  vim
)

for app in ${apps[@]}; do
  stow -v -R -t $HOME $app
done
