#! /usr/bin/env bash
mkdir -p "$ZDOTDIR"

ln -sf "$DOTFILES/zsh/zshenv" "$ZDOTDIR/.zshenv"
ln -sf "$DOTFILES/zsh/zshrc" "$ZDOTDIR/.zshrc"
