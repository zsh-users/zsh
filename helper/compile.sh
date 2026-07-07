#!/bin/sh
# compile.sh - Build zsh on Windows using the MSYS2 toolchain.
#
# Prerequisite: sh helper/install_build_tool.sh
#
# Usage (from Git Bash or any POSIX shell):
#   sh helper/compile.sh [source-dir]
#
# This is an out-of-tree (VPATH) build: all build output lands in <repo>/build
# and is kept after the build. The resulting binary is build/Src/zsh.exe.
# Generated autotools files in the source tree (configure, config.h.in,
# autom4te.cache, ...) are removed once the build finishes, so the source
# tree stays clean.
#
# The autotools build requires LF line endings. If the source tree has been
# converted to CRLF (e.g. by core.autocrlf=true), this script builds from a
# clean detached git worktree at ../zsh-build instead of the damaged tree.

set -e

REPO=$(cd "$(dirname "$0")/.." && pwd)
SRC="${1:-$REPO}"
BUILD="$REPO/build"

to_msys_path() {
    case "$1" in
        [A-Za-z]:/*|[A-Za-z]:\\*)
            if command -v cygpath >/dev/null 2>&1; then
                cygpath -u "$1"
            else
                drive=$(printf '%s' "$1" | sed 's#^\([A-Za-z]\):.*#\1#' | tr 'A-Z' 'a-z')
                rest=$(printf '%s' "$1" | sed 's#^[A-Za-z]:[/\\]*##; s#\\#/#g')
                printf '/%s/%s\n' "$drive" "$rest"
            fi
            ;;
        *)
            printf '%s\n' "$1"
            ;;
    esac
}

MSYS2_ROOT="$HOME/scoop/apps/msys2/current"
[ -x "$MSYS2_ROOT/usr/bin/bash.exe" ] || MSYS2_ROOT="$(to_msys_path "$USERPROFILE")/scoop/apps/msys2/current"
[ -x "$MSYS2_ROOT/usr/bin/bash.exe" ] || MSYS2_ROOT="/c/msys64"
MSYS2_BASH="$MSYS2_ROOT/usr/bin/bash.exe"

if [ ! -x "$MSYS2_BASH" ]; then
    echo "error: MSYS2 not found; run helper/install_build_tool.sh first" >&2
    exit 1
fi

# --- Guard against CRLF-damaged checkouts ----------------------------------
if [ -f "$SRC/configure.ac" ] && grep -q "$(printf '\r')" "$SRC/configure.ac"; then
    echo "==> Source tree has CRLF line endings; using clean worktree ../zsh-build"
    WT="$SRC/../zsh-build"
    if [ ! -d "$WT" ]; then
        git -C "$SRC" worktree add --detach "$WT" HEAD
    fi
    SRC=$(cd "$WT" && pwd)
fi

SRC_MSYS=$(to_msys_path "$SRC")
BUILD_MSYS=$(to_msys_path "$BUILD")

echo "==> Source tree:  $SRC"
echo "==> Build output: $BUILD"
mkdir -p "$BUILD_MSYS"

# --- preconfig, configure (in build/), make ---------------------------------
"$MSYS2_BASH" -lc "
    set -e
    export PATH=/usr/bin:\$PATH
    export TMPDIR='$BUILD_MSYS/tmp'
    mkdir -p \"\$TMPDIR\"
    cd '$SRC_MSYS'

    # a leftover in-tree build breaks VPATH builds; clear it first
    if [ -f config.status ]; then
        echo '==> Removing leftover in-tree build (make distclean)...'
        make distclean >/dev/null 2>&1 || true
    fi

    if [ ! -f configure ]; then
        echo '==> Generating configure (Util/preconfig)...'
        ./Util/preconfig
    fi

    cd '$BUILD_MSYS'
    # re-run configure serially if it was regenerated, otherwise parallel
    # sub-makes race to reconfigure and corrupt each other's conftest files
    if [ ! -f config.status ] || [ '$SRC_MSYS/configure' -nt config.status ]; then
        echo '==> Running configure (out-of-tree)...'
        # config.guess reports mingw32, not cygwin, so configure picks bare
        # ld for module linking; modern binutils then exports no symbols and
        # configure silently disables dynamic modules. Preset the link
        # command the cygwin branch would use.
        DLLD=gcc DLLDFLAGS='-shared -Wl,--export-all-symbols' \
            '$SRC_MSYS/configure' --prefix=/usr/local
    fi

    echo '==> Running make...'
    make -j\$(nproc)

    # --- Assemble a portable runtime in build/bin ---------------------------
    # zsh.exe + libzsh + loadable modules + the MSYS2 runtime DLLs they need,
    # so the result runs from any shell (Git Bash, cmd, ...) without MSYS2
    # on PATH. Modules go in bin/zsh/ because module 'zsh/foo' is looked up
    # as <module_path>/zsh/foo.dll.
    echo '==> Assembling portable runtime in build/bin...'
    cd '$BUILD_MSYS'
    rm -rf bin
    mkdir -p bin
    cp Src/zsh.exe Src/libzsh-*.dll bin/

    sed -n 's/^name=\([^ ]*\).* modfile=\([^ ]*\).* link=dynamic .*/\1 \2/p' config.modules \\
        | while read -r modname modfile; do
            src=\"\${modfile%.mdd}.dll\"
            dll=\"\${modfile##*/}\"
            dll=\"\${dll%.mdd}.dll\"
            dest=\"bin/\$modname.dll\"
            if [ -f \"\$src\" ]; then
                mkdir -p \"\$(dirname \"\$dest\")\"
                cp \"\$src\" \"\$dest\"
            else
                echo \"warning: expected module not found: \$src\" >&2
            fi
        done

    printf '%s\n' \\
        '@echo off' \\
        'set "ZSH_PORTABLE_DIR=%~dp0"' \\
        'set "ZSH_WIN_HOME=%USERPROFILE%"' \\
        'set "ZSH_TERMINFO_DIR=%ZSH_PORTABLE_DIR:\=/%share/terminfo"' \\
        'set "ZSH_TERMINFO_DRIVE=%ZSH_TERMINFO_DIR:~0,1%"' \\
        'set "ZSH_TERMINFO_PATH=%ZSH_TERMINFO_DIR:~2%"' \\
        'set "TERMINFO=/cygdrive/%ZSH_TERMINFO_DRIVE%%ZSH_TERMINFO_PATH%"' \\
        'set "PATH=%ZSH_PORTABLE_DIR%;%PATH%"' \\
        'if defined ZDOTDIR (' \\
        '    set "ZSH_ORIG_ZDOTDIR=%ZDOTDIR%"' \\
        ') else (' \\
        '    set "ZSH_ORIG_ZDOTDIR=%USERPROFILE%"' \\
        ')' \\
        'set "ZDOTDIR=%ZSH_PORTABLE_DIR%"' \\
        '"%ZSH_PORTABLE_DIR%zsh.exe" %*' \\
        > bin/zsh.cmd

    printf '%s\n' \\
        'zsh_portable_dir=${ZSH_PORTABLE_DIR:-}' \\
        'if [[ -n $zsh_portable_dir ]]; then' \\
        '  zsh_portable_dir=${zsh_portable_dir//\\\\//}' \\
        '  zsh_portable_dir=${zsh_portable_dir%/}' \\
        '  if [[ $zsh_portable_dir == [A-Za-z]:/* ]]; then' \\
        '    zsh_portable_dir="/cygdrive/${(L)zsh_portable_dir[1]}/${zsh_portable_dir[4,-1]}"' \\
        '  fi' \\
        '  module_path=("$zsh_portable_dir" $module_path)' \\
        '  TERMINFO="$zsh_portable_dir/share/terminfo"' \\
        '  export TERMINFO' \\
        'fi' \\
        'if [[ -n ${ZSH_WIN_HOME:-} ]]; then' \\
        '  HOME=${ZSH_WIN_HOME//\\\\//}' \\
        '  export HOME' \\
        'fi' \\
        'if [[ -o interactive ]]; then' \\
        '  PROMPT="%n@%~%# "' \\
        '  zsh_portable_fix_keys() {' \\
        '    local zsh_portable_keymap' \\
        '    for zsh_portable_keymap in main emacs viins; do' \\
        '      bindkey -M "$zsh_portable_keymap" "^?" backward-delete-char 2>/dev/null' \\
        '      bindkey -M "$zsh_portable_keymap" "^H" backward-delete-char 2>/dev/null' \\
        '    done' \\
        '    stty erase "^?" 2>/dev/null || stty erase "^H" 2>/dev/null' \\
        '  }' \\
        '  precmd_functions=(${precmd_functions:#zsh_portable_fix_keys} zsh_portable_fix_keys)' \\
        'fi' \\
        '' \\
        'if [[ -n ${ZSH_ORIG_ZDOTDIR:-} ]]; then' \\
        '  zsh_orig_zdotdir=${ZSH_ORIG_ZDOTDIR//\\\\//}' \\
        '  ZDOTDIR=$zsh_orig_zdotdir' \\
        '  export ZDOTDIR' \\
        '  if [[ $ZDOTDIR != $zsh_portable_dir && -r $ZDOTDIR/.zshenv ]]; then' \\
        '    source $ZDOTDIR/.zshenv' \\
        '  fi' \\
        '  unset ZSH_ORIG_ZDOTDIR ZSH_PORTABLE_DIR ZSH_WIN_HOME zsh_portable_dir zsh_orig_zdotdir' \\
        'fi' \\
        > bin/.zshenv

    { ldd Src/zsh.exe; find Src -name '*.dll' -exec ldd {} +; } 2>/dev/null \\
        | awk '/=> \/usr\/bin\/msys-/ { print \$3 }' | sort -u \\
        | while read -r dll; do cp \"\$dll\" bin/; done

    if command -v tput.exe >/dev/null 2>&1; then
        cp \"\$(command -v tput.exe)\" bin/
    fi
    if [ -d /usr/share/terminfo ]; then
        mkdir -p bin/share
        cp -R /usr/share/terminfo bin/share/
    fi
"

# --- Stamp the build: zsh version + source commit ---------------------------
{
    "$BUILD/bin/zsh.exe" --version
    git -C "$SRC" log -1 --format='%H'
} > "$BUILD/bin/version.txt"
echo "==> version.txt:"
cat "$BUILD/bin/version.txt"

# --- Clean generated files out of the source tree; keep build/ --------------
echo "==> Cleaning generated files from source tree..."
rm -rf "$SRC/autom4te.cache"
rm -f "$SRC/configure" "$SRC/config.h.in" "$SRC/stamp-h.in" "$SRC/META-FAQ"

echo "==> Build complete. Output kept in: $BUILD"
echo "==> Portable runtime: $BUILD/bin (zsh.cmd, zsh.exe, required DLLs, modules)"
"$BUILD/bin/zsh.exe" --version
echo "==> For dynamic modules (zle etc.) outside MSYS2, run $BUILD/bin/zsh.cmd"
