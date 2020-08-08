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
)

for app in ${apps[@]}; do
  stow -v -R -t $HOME $app
done

# Symlinks for other targets
stow -v -R -t "$HOME/Library/Application Support/Code/User" vscode
