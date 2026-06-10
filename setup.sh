#!/bin/bash
# Symlink all conf repo dotfiles into place.
# Safe to re-run — backs up existing files before overwriting.

set -e

CONF_DIR="$(cd "$(dirname "$0")" && pwd)"

link() {
    local src="$CONF_DIR/$1"
    local dest="$HOME/$2"

    if [ ! -e "$src" ]; then
        echo "SKIP $src (not found)"
        return
    fi

    if [ -e "$dest" ] && [ ! -L "$dest" ]; then
        echo "BACKUP $dest -> ${dest}.bak"
        mv "$dest" "${dest}.bak"
    fi

    ln -sfn "$src" "$dest"
    echo "LINK $dest -> $src"
}

# Emacs
link .spacemacs .spacemacs

# Tmux
link .tmux.conf .tmux.conf

# Prezto / Zsh
link zprezto/zshrc .zshrc
link zprezto/zshenv .zshenv
link zprezto/zpreztorc .zpreztorc
link zprezto/zprofile .zprofile

# Prezto runcoms (so prezto itself sees the configs)
if [ -d "$HOME/.zprezto/runcoms" ]; then
    link zprezto/zshrc .zprezto/runcoms/zshrc
    link zprezto/zshenv .zprezto/runcoms/zshenv
    link zprezto/zpreztorc .zprezto/runcoms/zpreztorc
    link zprezto/zprofile .zprezto/runcoms/zprofile
else
    echo "WARN ~/.zprezto/runcoms not found — install prezto first"
fi

echo "Done."
