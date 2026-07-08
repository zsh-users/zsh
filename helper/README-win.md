# Building zsh on Windows

zsh is a POSIX shell and cannot be built with MSVC alone; it needs the MSYS2
(Cygwin-derived) POSIX runtime and toolchain. These scripts automate that.

## Prerequisites

- [scoop](https://scoop.sh) and/or [winget](https://learn.microsoft.com/windows/package-manager/winget/)
  available on PATH. `install_build_tool.sh` will install scoop via winget if
  it's missing.
- Run scripts from Git Bash or another POSIX shell (not PowerShell/cmd).

## Usage

```sh
sh helper/install_build_tool.sh   # one-time: installs MSYS2 + gcc/make/autoconf/automake/ncurses-devel
sh helper/compile.sh              # builds zsh into ./build
sh helper/scoop_install.sh        # optional: install the result as a scoop app with a `zsh` shim
```

### install_build_tool.sh

Installs, in order:

1. **scoop** (via winget, if not already present)
2. **MSYS2** (via scoop, falling back to winget) at
   `~/scoop/apps/msys2/current` (or `C:\msys64` if installed via winget)
3. Build tools inside MSYS2 via `pacman`: `gcc`, `make`, `autoconf`,
   `automake`, `ncurses-devel`

These are the plain `msys/*` packages (POSIX runtime), not the
`mingw-w64-*` cross-toolchain packages — zsh's autotools build expects a
Cygwin-like environment.

### compile.sh

Builds zsh out-of-tree so the source directory stays clean:

1. Runs `Util/preconfig` (autoconf) in the source tree if `configure` doesn't
   exist yet.
2. Runs `configure` and `make -j$(nproc)` **inside `build/`** (VPATH build),
   not in the source tree.
3. Assembles a **portable runtime in `build/bin`**: `zsh.exe`, `zsh.cmd`,
   `libzsh-*.dll`, all dynamic modules from `config.modules`, a bootstrap
   `.zshenv` for the wrapper, MSYS2 runtime DLLs discovered via `ldd`,
   `tput.exe`, and the MSYS2 terminfo database.
4. Removes autotools-generated files from the source tree afterward
   (`configure`, `config.h.in`, `autom4te.cache`, `stamp-h.in`, `META-FAQ`).
5. Leaves `build/` in place — it is **not** cleaned up.

**Build output**: `build/bin/zsh.cmd` is the supported portable entry point.
It launches `build/bin/zsh.exe` after setting up the runtime environment.
Intermediate build artifacts remain under `build/`.

**CRLF guard**: if the source tree's line endings have been converted to
CRLF (e.g. by `core.autocrlf=true`), the autotools build breaks. In that
case the script detects the damaged `configure.ac` and instead builds from a
clean detached git worktree at `../zsh-build`, leaving your source tree
untouched. Fix `core.autocrlf` and restore LF endings to avoid this path.

## Running the built binary

`build/bin` is self-contained — the required MSYS2 DLLs sit next to
`zsh.exe`, so it runs from Git Bash, cmd, or PowerShell without MSYS2 on
`PATH`:

```sh
build/bin/zsh.cmd --version
```

Dynamic modules (`zsh/zle`, `zsh/complete`, ...) are compiled to look in
`/usr/local/lib/zsh/<version>`, which won't exist outside MSYS2. Use the
portable `zsh.cmd` wrapper so the packaged `.zshenv` can prepend `build/bin`
to `module_path`, then restore your real `ZDOTDIR`:

```sh
build/bin/zsh.cmd
```

Do **not** run the bare `build/Src/zsh.exe` from Git Bash — it will fail
with error 0xc0000135 because Git Bash doesn't ship the MSYS2 runtime DLLs
(its own `msys-2.0.dll` is a different, incompatible build and it has no
`msys-ncursesw6.dll`).

The wrapper/bootstrap also:

- sets `HOME` from Windows `%USERPROFILE%`, so `~/` resolves to your Windows
  profile instead of a missing `/home/<user>` directory;
- sets `TERMINFO` before zsh starts and packages `share/terminfo`, so `tput`
  and terminals such as `xterm-256color` work;
- prepends `build/bin` to `PATH`, so packaged helper tools such as `tput.exe`
  are found;
- sets the default interactive prompt to `username@current-path`;
- binds both common Backspace sequences (`^?` and `^H`) in `main`, `emacs`,
  and `viins` keymaps. If Backspace still behaves exactly like Tab, the
  terminal is likely sending literal `^I`, which must be fixed in the
  terminal profile/keybinding.

Note: `make install` into the MSYS2 prefix currently fails at
`install.modules` (a `rlimits` module link error against static libc on
MSYS); use the portable `build/bin` layout instead.

## Installing as a scoop app (scoop_install.sh)

`sh helper/scoop_install.sh` packages `build/bin` and installs it through
scoop, using the manifest in `bucket/zsh.json`:

1. Zips `build/bin/*` to `build/release/zsh.zip` (with Windows' bsdtar —
   PowerShell's `Compress-Archive` cannot read the MSYS2-built binaries).
2. Regenerates `bucket/zsh.json` with the version, a `file:///` URL to the
   zip, and its sha256 hash.
3. Runs `scoop install bucket/zsh.json` (uninstalling any previous zsh
   first), which:
   - extracts the zip to `~/scoop/apps/zsh/<version>\` and links
     `~/scoop/apps/zsh/current` to it — this is scoop's canonical location,
     derived from the manifest filename and `version` field;
   - creates `~/scoop/shims/zsh` and `~/scoop/shims/zsh.cmd` from the
     manifest's `zsh.cmd` entry, so `zsh` works from any shell with scoop's
     shims on `PATH`;
   - uses the wrapper and packaged `.zshenv` to set `module_path`, `HOME`,
     `TERMINFO`, prompt defaults, and Backspace bindings without relying on
     the ignored `MODULE_PATH` environment variable.

The installer clears Scoop's `zsh` download cache before reinstalling, because
the local `file:///` zip is regenerated on each package run.

To rebuild and reinstall after source changes:
`sh helper/compile.sh && sh helper/scoop_install.sh`.
To remove: `scoop uninstall zsh`.

## Known non-fatal warnings

During `make`, you may see:

```
sh: line 1: man: command not found
sh: line 1: nroff: command not found
```

These only affect regeneration of `Doc/help.txt` (interactive `run-help`
text) and don't fail the build. Install `man`/`groff` via pacman if you need
that file regenerated.
