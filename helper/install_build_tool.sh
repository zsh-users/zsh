#!/bin/sh
# install_build_tool.sh - Install the toolchain needed to build zsh on Windows.
#
# zsh is a POSIX shell and cannot be built with MSVC/MinGW alone; it needs the
# MSYS2 (Cygwin-derived) POSIX runtime. This script installs:
#   1. scoop        (via winget, if missing)
#   2. MSYS2        (via scoop, with winget fallback)
#   3. build tools  (gcc, make, autoconf, automake, ncurses-devel via pacman)
#
# Run from Git Bash or any POSIX shell on Windows:
#   sh helper/install_build_tool.sh

set -e

# --- 1. scoop -------------------------------------------------------------
if ! command -v scoop >/dev/null 2>&1; then
    echo "==> Installing scoop via winget..."
    winget install --id ScoopInstaller.Scoop -e --accept-source-agreements \
        --accept-package-agreements
else
    echo "==> scoop already installed"
fi

# --- 2. MSYS2 -------------------------------------------------------------
MSYS2_ROOT="$HOME/scoop/apps/msys2/current"
if [ ! -x "$MSYS2_ROOT/usr/bin/bash.exe" ]; then
    echo "==> Installing MSYS2..."
    if command -v scoop >/dev/null 2>&1; then
        scoop install msys2
    else
        winget install --id MSYS2.MSYS2 -e --accept-source-agreements \
            --accept-package-agreements
        MSYS2_ROOT="/c/msys64"
    fi
else
    echo "==> MSYS2 already installed at $MSYS2_ROOT"
fi

# --- 3. First-run setup + pacman keyring ------------------------------------
# A fresh MSYS2 install must be started once to create /etc, HOME, and the
# pacman keyring; without it pacman fails with "Public keyring not found" /
# "keyring is not writable" / PGP signature errors. pacman-key is a script
# inside MSYS2, so it must run via MSYS2's bash, not PowerShell/cmd.
"$MSYS2_ROOT/usr/bin/bash.exe" -lc 'true'
if [ ! -d "$MSYS2_ROOT/etc/pacman.d/gnupg" ]; then
    echo "==> Initializing pacman keyring (first run)..."
    "$MSYS2_ROOT/usr/bin/bash.exe" -lc \
        'pacman-key --init && pacman-key --populate msys2'
fi

# --- 4. Build tools inside MSYS2 -------------------------------------------
# NOTE: use the plain 'msys' packages (POSIX runtime), NOT mingw-w64-* ones.
echo "==> Installing gcc, make, autoconf, automake, ncurses-devel..."
"$MSYS2_ROOT/usr/bin/pacman.exe" -Sy --noconfirm --needed \
    gcc make autoconf automake ncurses-devel

echo "==> Done. Toolchain check:"
"$MSYS2_ROOT/usr/bin/bash.exe" -lc \
    'export PATH=/usr/bin:$PATH; gcc --version | head -1; make --version | head -1; autoconf --version | head -1'

echo "==> Next step: sh helper/compile.sh"
