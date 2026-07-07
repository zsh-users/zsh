#!/bin/sh
# scoop_install.sh - Package the portable build/bin runtime and install it
# via scoop, so zsh lands in scoop's standard app location with a shim.
#
# Prerequisite: sh helper/compile.sh  (produces build/bin)
#
# Usage (from Git Bash or any POSIX shell):
#   sh helper/scoop_install.sh
#
# What it does:
#   1. Zips build/bin/*  ->  build/zsh-<version>-x64.zip
#   2. Regenerates bucket/zsh.json with the version and sha256 hash
#      (file:/// url pointing at the zip)
#   3. scoop install bucket/zsh.json, which:
#        - extracts to  ~/scoop/apps/zsh/<version>\   (+ 'current' junction)
#        - creates the shim  ~/scoop/shims/zsh.exe    (from zsh.cmd)
#        - uses the packaged .zshenv bootstrap so zsh finds dynamic modules

set -e

REPO=$(cd "$(dirname "$0")/.." && pwd)
BUILD="$REPO/build"
BUCKET="$REPO/bucket"

to_windows_path() {
    case "$1" in
        [A-Za-z]:/*)
            printf '%s\n' "$1"
            ;;
        [A-Za-z]:\\*)
            printf '%s\n' "$1" | sed 's#\\#/#g'
            ;;
        *)
            if command -v cygpath >/dev/null 2>&1; then
                cygpath -m "$1"
            else
                case "$1" in
                    /[A-Za-z]/*)
                        drive=$(printf '%s' "$1" | sed 's#^/\([A-Za-z]\)/.*#\1#' | tr 'a-z' 'A-Z')
                        rest=$(printf '%s' "$1" | sed 's#^/[A-Za-z]/##')
                        printf '%s:/%s\n' "$drive" "$rest"
                        ;;
                    *)
                        printf '%s\n' "$1"
                        ;;
                esac
            fi
            ;;
    esac
}

find_windows_tar() {
    for tar in \
        /c/Windows/System32/tar.exe \
        C:/Windows/System32/tar.exe \
        /mnt/c/Windows/System32/tar.exe
    do
        if [ -x "$tar" ]; then
            printf '%s\n' "$tar"
            return 0
        fi
    done

    if command -v tar.exe >/dev/null 2>&1; then
        command -v tar.exe
        return 0
    fi

    return 1
}

if [ ! -x "$BUILD/bin/zsh.exe" ]; then
    echo "error: $BUILD/bin/zsh.exe not found; run helper/compile.sh first" >&2
    exit 1
fi
if [ ! -f "$BUILD/bin/zsh.cmd" ] || [ ! -f "$BUILD/bin/zsh/zle.dll" ]; then
    echo "error: build/bin is missing zsh.cmd or zsh/zle.dll; rerun helper/compile.sh" >&2
    exit 1
fi
command -v scoop >/dev/null 2>&1 || {
    echo "error: scoop not found; run helper/install_build_tool.sh first" >&2
    exit 1
}

VERSION=$("$BUILD/bin/zsh.exe" --version | awk '{print $2}')
RELEASE="$BUILD/release"
ZIP="$RELEASE/zsh.zip"
REPO_WIN=$(to_windows_path "$REPO")   # Windows-style path (C:/...)
TAR_EXE=$(find_windows_tar) || {
    echo "error: Windows tar.exe not found; expected it under C:/Windows/System32" >&2
    exit 1
}

# Public download URL for scoop users: the git origin remote, develop branch.
# (Requires build/release/zsh.zip to be committed and pushed on develop.)
ORIGIN=$(git -C "$REPO" remote get-url origin | sed -e 's/\.git$//')
PUBLIC_URL=$(printf '%s' "$ORIGIN" \
    | sed -e 's#github\.com#raw.githubusercontent.com#')/develop/build/release/zsh.zip

# --- 1. Zip the portable runtime --------------------------------------------
# Windows' bsdtar is used because PowerShell's Compress-Archive cannot read
# the MSYS2-built binaries.
echo "==> Packaging build/bin -> build/release/zsh.zip"
mkdir -p "$RELEASE"
rm -f "$ZIP"
"$TAR_EXE" -a -cf "$REPO_WIN/build/release/zsh.zip" \
    -C "$REPO_WIN/build/bin" .
[ -f "$ZIP" ] || { echo "error: zip creation failed" >&2; exit 1; }

HASH=$(sha256sum "$ZIP" | awk '{print $1}')
[ -n "$HASH" ] || { echo "error: could not hash zip" >&2; exit 1; }
echo "==> sha256: $HASH"

# --- 2. Regenerate the scoop manifest ----------------------------------------
mkdir -p "$BUCKET"
cat > "$BUCKET/zsh.json" <<EOF
{
    "version": "$VERSION",
    "description": "Zsh shell built from source with the MSYS2 toolchain (portable runtime)",
    "homepage": "https://www.zsh.org",
    "license": "Zsh (MIT-like)",
    "architecture": {
        "64bit": {
            "url": "$PUBLIC_URL",
            "hash": "$HASH"
        }
    },
    "bin": [
        [
            "zsh.cmd",
            "zsh"
        ]
    ],
    "notes": "zsh built from source with MSYS2; zip served from the develop branch of $ORIGIN."
}
EOF
echo "==> Wrote $BUCKET/zsh.json (url: $PUBLIC_URL)"

# A local-file variant of the manifest, so the install can be tested before
# the zip is committed and pushed to origin/develop.
mkdir -p "$BUILD/local-manifest"
sed "s#\"url\": \".*\"#\"url\": \"file:///$REPO_WIN/build/release/zsh.zip\"#" \
    "$BUCKET/zsh.json" > "$BUILD/local-manifest/zsh.json"

# --- 3. Install through scoop ------------------------------------------------
if scoop list zsh 2>/dev/null | grep -q '^zsh '; then
    echo "==> Removing previously installed zsh..."
    scoop uninstall zsh
fi
echo "==> Clearing scoop download cache for zsh..."
scoop cache rm zsh 2>/dev/null || true
echo "==> Installing via scoop (from local zip)..."
scoop install "$REPO_WIN/build/local-manifest/zsh.json"

# --- 4. Verify ---------------------------------------------------------------
echo "==> Installed. Shim check:"
SHIM="$HOME/scoop/shims/zsh"
if [ ! -x "$SHIM" ] && [ -x "$HOME/scoop/shims/zsh.cmd" ]; then
    SHIM="$HOME/scoop/shims/zsh.cmd"
fi
"$SHIM" --version
echo "==> App dir: $HOME/scoop/apps/zsh/current"
echo "==> zsh.cmd bootstraps module_path for dynamic modules; run: zsh"
