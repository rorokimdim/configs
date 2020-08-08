#!/usr/bin/env bash

apps=(
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
