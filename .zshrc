# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Theme
ZSH_THEME="cypher"

plugins=(git branch git-prompt)

source $ZSH/oh-my-zsh.sh

# Disable bracketed paste mode
unset zle_bracketed_paste

# Ensure active python virtualenv is on the prompt
export VIRTUAL_ENV_DISABLE_PROMPT=0

function virtualenv_info {
    [ $VIRTUAL_ENV ] && echo '('$(basename $VIRTUAL_ENV)') '
}

# Cypher theme prompt customization
setopt PROMPT_SUBST
local return_code="%(?..%{$fg_bold[red]%}%? ↵%{$reset_color%})"
PROMPT='%{$fg[yellow]%}$(virtualenv_info)%{$reset_color%}% %{${fg[green]}%}%3~%(0?. . %{${fg[red]}%}%? )$(git_prompt_info)%{$reset_color%}%{${fg[blue]}%}»%{${reset_color}%} '
RPS1="${return_code}"

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[yellow]%}("
ZSH_THEME_GIT_PROMPT_SUFFIX=") %{$reset_color%}"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%%"
ZSH_THEME_GIT_PROMPT_ADDED="+"
ZSH_THEME_GIT_PROMPT_MODIFIED="*"
ZSH_THEME_GIT_PROMPT_RENAMED="~"
ZSH_THEME_GIT_PROMPT_DELETED="!"
ZSH_THEME_GIT_PROMPT_UNMERGED="?"

bindkey "^R" history-incremental-search-backward
bindkey "\e[A" history-beginning-search-backward
bindkey "\e[B" history-beginning-search-forward
bindkey '\ef' emacs-forward-word
bindkey '\eb' emacs-backward-word

# History settings
export HISTSIZE=999999999
export SAVEHIST=999999999
export HISTFILE=${HOME}/.zsh_history
setopt APPEND_HISTORY INC_APPEND_HISTORY
setopt BANG_HIST
setopt EXTENDED_HISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_SAVE_NO_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY

export GIT_EDITOR="emacs"
export EDITOR="emacs"

# Ensure jump-by-word works as expected
export WORDCHARS="/\\\()\"'-.,:;<>~\!@#$%^&*|+=[]{}~?|"

# bash-my-aws
if [ -d ${HOME}/.bash-my-aws ]; then
    for f in ${HOME}/.bash-my-aws/lib/*-functions; do source $f; done
fi

export PATH="/usr/local/bin/:${PATH}:${HOME}/bin/:/usr/sbin/:${HOME}/go/bin:${HOME}/.local/bin"

# Enable rust cargo binaries
export PATH="${HOME}/.cargo/bin:${PATH}"
if [[ ":$PATH:" != *":/usr/share/java/gradle/bin:"* ]]; then
    export PATH="/usr/share/java/gradle/bin:${PATH}"
fi

export VCPKG_ROOT=~/git/vcpkg

# Ensure a directory appears at the front of PATH exactly once.
path_prepend_unique() {
    local dir="$1"
    [ -z "$dir" ] && return
    dir="${dir%/}"
    local path_var="${2:-PATH}"
    eval "local current=\"\${$path_var}\""
    local updated
    if command -v python3 >/dev/null 2>&1; then
        updated=$(PATH_TO_UPDATE="$current" python3 - "$dir" <<'PY'
import os, sys
target = sys.argv[1]
current = os.environ.get("PATH_TO_UPDATE", "")
parts = [p for p in current.split(':') if p and p != target]
if parts:
    sys.stdout.write(target + ':' + ':'.join(parts))
else:
    sys.stdout.write(target)
PY
)
    else
        local old_ifs="$IFS"
        if [ -n "${ZSH_VERSION:-}" ]; then
            setopt local_options sh_word_split
        fi
        IFS=':'
        updated="$dir"
        for part in $current; do
            if [ -z "$part" ] || [ "$part" = "$dir" ]; then
                continue
            fi
            updated="$updated:$part"
        done
        IFS="$old_ifs"
    fi
    eval "export $path_var=\"$updated\""
}

# OS-specific configuration
unameOut="$(uname -s)"
case "${unameOut}" in
Linux*)
    machine=Linux
    export PYENV_ROOT="$HOME/.pyenv"

    # --- LINUX ANDROID SETUP ---
    export ANDROID_HOME="$HOME/Android/Sdk"
    export ANDROID_SDK_ROOT="$ANDROID_HOME"

    # Helper to find latest version (Linux specific)
    __android_pick_latest_linux() {
        local base="$1"
        [ ! -d "$base" ] && return 1
        find "$base" -maxdepth 1 -mindepth 1 -type d 2>/dev/null | sort -V | tail -n 1
    }

    # Detect NDK
    local ndk_root=$(__android_pick_latest_linux "$ANDROID_HOME/ndk")
    if [ -n "$ndk_root" ]; then
        export ANDROID_NDK_HOME="$ndk_root"
        export ANDROID_NDK_ROOT="$ndk_root"
    fi

    # Detect CMake
    local cmake_root=$(__android_pick_latest_linux "$ANDROID_HOME/cmake")
    if [ -n "$cmake_root" ] && [ -d "$cmake_root/bin" ]; then
        export ANDROID_CMAKE_BIN="$cmake_root/bin/cmake"
        # Prepend CMake bin to PATH
        path_prepend_unique "$cmake_root/bin"
    fi

    unset -f __android_pick_latest_linux

    # Java Setup (Ubuntu OpenJDK 17)
    if [ -d "/usr/lib/jvm/java-17-openjdk-amd64" ]; then
        export JAVA_HOME="/usr/lib/jvm/java-17-openjdk-amd64"
    fi

    # Update PATH with standard Android tools
    path_prepend_unique "$ANDROID_HOME/cmdline-tools/latest/bin"
    path_prepend_unique "$ANDROID_HOME/platform-tools"
    path_prepend_unique "$ANDROID_HOME/emulator"

    # --- LINUX GO SETUP ---
    # 1. Try to find system installed Go (smart detection)
    GO_LATEST_BIN=$(ls -d /usr/lib/go-*/bin 2>/dev/null | sort -V | tail -n 1)
    if [ -n "$GO_LATEST_BIN" ]; then
        path_prepend_unique "$GO_LATEST_BIN"
    fi

    # 2. Standard Workspace Setup
    export GOPATH="${HOME}/go"
    path_prepend_unique "${GOPATH}/bin"
    export GOPROXY="https://proxy.golang.org,direct"
    export GOSUMDB="sum.golang.org"

    # Node / NVM
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
    ;;

Darwin*)
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="/usr/local/opt/coreutils/bin/:${PATH}"
    export PATH="/opt/homebrew/bin/:${PATH}"
    export PATH="/usr/local/opt/texinfo/bin/:${PATH}"

    export NVM_DIR="$HOME/.nvm"
    [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"
    [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"

    NETSKOPE_CERT_BUNDLE="/opt/netskope-cert-bundle.pem"
    REQUESTS_CA_BUNDLE="/opt/netskope-cert-bundle.pem"
    export AWS_CA_BUNDLE="/opt/netskope-cert-bundle.pem"
    machine=Mac
    ;;
CYGWIN*)
    machine=Cygwin
    ;;
MSYS_NT*|MINGW*|MINGW64_NT*)
    machine=MSYS2
    export MSYS=winsymlinks:native
    export PYENV_ROOT="/c/Users/mkusp/.pyenv/pyenv-win"

    # Go Setup (Windows/MSYS)
    if command -v go >/dev/null 2>&1; then
        go_binary=$(command -v go)
        go_root_unix=$(dirname "$(dirname "$go_binary")")
        if command -v cygpath >/dev/null 2>&1; then
            goroot_converted=$(cygpath -w "$go_root_unix" 2>/dev/null || printf '%s' "$go_root_unix")
            export GOROOT="$goroot_converted"
        else
            export GOROOT="$go_root_unix"
        fi

        gopath_unix="${HOME}/go"
        if [ ! -d "$gopath_unix" ]; then mkdir -p "$gopath_unix"; fi
        if command -v cygpath >/dev/null 2>&1; then
            export GOPATH="$(cygpath -w "$gopath_unix" 2>/dev/null || printf '%s' "$gopath_unix")"
        else
            export GOPATH="$gopath_unix"
        fi

        export PATH="${gopath_unix}/bin:$PATH"
        export GOPROXY="https://proxy.golang.org,direct"
        export GOSUMDB="sum.golang.org"
    fi

    export MINGW_ORIGINAL_PATH=$PATH

    function mingw_clear_external_build_path() {
        export PATH=$(echo ${PATH} | awk -v RS=: -v ORS=: '/c\// {next} {print}' | sed 's/:*$//')
    }

    if [ -n "${STARTDIR:-}" ]; then
        safe_start_dir="$STARTDIR"
        if command -v cygpath >/dev/null 2>&1; then
            safe_start_dir=$(cygpath "$STARTDIR" 2>/dev/null || printf '%s' "$STARTDIR")
        fi
        if [ -n "$safe_start_dir" ] && [ -d "$safe_start_dir" ]; then
            cd "$safe_start_dir"
        fi
    fi

    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

    # Add Docker Desktop and Chocolatey at the end of PATH
    if [ -d "/c/Program Files/Docker/Docker/resources/bin" ]; then
        export PATH="$PATH:/c/Program Files/Docker/Docker/resources/bin"
    fi
    if [ -d "/c/ProgramData/chocolatey/bin" ]; then
        export PATH="$PATH:/c/ProgramData/chocolatey/bin"
    fi

    # Android toolchain paths for Windows shells
    ANDROID_BASE_DIR="/c/Android"
    export ANDROID_HOME="$ANDROID_BASE_DIR/android-sdk"
    export ANDROID_SDK_ROOT="$ANDROID_HOME"
    export ANDROID_PLATFORM_TOOLS_DIR="$ANDROID_HOME/platform-tools"

    __android_pick_latest_dir() {
        local base="$1"
        if [ ! -d "$base" ]; then
            return 1
        fi
        if command -v python3 >/dev/null 2>&1; then
            python3 - "$base" <<'PY'
import os
import sys

base = sys.argv[1]
dirs = []
for name in os.listdir(base):
    path = os.path.join(base, name)
    if os.path.isdir(path):
        dirs.append(path)

def version_key(path):
    parts = []
    for chunk in os.path.basename(path).replace('-', '.').split('.'):
        if not chunk:
            continue
        if chunk.isdigit():
            parts.append(int(chunk))
        else:
            parts.append(chunk)
    return parts

if dirs:
    dirs.sort(key=version_key)
    sys.stdout.write(dirs[-1])
PY
        else
            find "$base" -maxdepth 1 -mindepth 1 -type d 2>/dev/null | sort -V 2>/dev/null | tail -n 1
        fi
    }

    ndk_base="$ANDROID_HOME/ndk"
    ndk_root=$(__android_pick_latest_dir "$ndk_base")
    ndk_path=""
    if [ -n "$ndk_root" ]; then
        if command -v cygpath >/dev/null 2>&1; then
            ndk_path=$(cygpath -m "$ndk_root" 2>/dev/null || printf '%s' "$ndk_root")
        else
            ndk_path="$ndk_root"
        fi
    fi

    if [ -n "$ndk_path" ]; then
        export ANDROID_NDK_HOME="$ndk_path"
        export ANDROID_NDK_ROOT="$ndk_path"
    fi

    cmake_base="$ANDROID_HOME/cmake"
    cmake_root=$(__android_pick_latest_dir "$cmake_base")
    cmake_bin=""
    if [ -n "$cmake_root" ] && [ -d "$cmake_root/bin" ]; then
        cmake_bin="$cmake_root/bin"
    fi

    if [ -n "$cmake_bin" ]; then
        export ANDROID_CMAKE_PATH_POSIX="$cmake_bin"
        if command -v cygpath >/dev/null 2>&1; then
            cmake_bin_win=$(cygpath -m "$cmake_bin" 2>/dev/null || printf '%s' "$cmake_bin")
        else
            cmake_bin_win="$cmake_bin"
        fi
        export ANDROID_CMAKE_BIN="$cmake_bin_win"
    fi

    unset -f __android_pick_latest_dir 2>/dev/null
    setopt null_glob
    jdk_candidates=("$ANDROID_BASE_DIR"/openjdk/jdk-*)
    unsetopt null_glob
    if (( ${#jdk_candidates[@]} )); then
        export JAVA_HOME="${jdk_candidates[-1]}"
    else
        export JAVA_HOME="$ANDROID_BASE_DIR/openjdk"
    fi
    unset jdk_candidates
    ;;
*)
    machine="UNKNOWN:${unameOut}"
    ;;
esac

# Pyenv setup
if which pyenv; then
    PYENV=pyenv
    export PYENV_VIRTUALENV_DISABLE_PROMPT=1
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
    if [[ ! ${PYENV_ROOT} =~ "pyenv-win" ]]; then
        eval "$(pyenv virtualenv-init -)"
    fi
fi &>/dev/null

# Keep Android tooling ahead of other PATH entries when available
if [ -n "${ANDROID_CMAKE_PATH_POSIX:-}" ]; then
    path_prepend_unique "$ANDROID_CMAKE_PATH_POSIX"
fi
if [ -n "${ANDROID_PLATFORM_TOOLS_DIR:-}" ]; then
    path_prepend_unique "$ANDROID_PLATFORM_TOOLS_DIR"
fi

git-rename() {
    NEW_BRANCH_NAME=$1
    OLD_BRANCH_NAME=$(git rev-parse --abbrev-ref HEAD)
    git branch -m $OLD_BRANCH_NAME $NEW_BRANCH_NAME
    git push origin --delete $OLD_BRANCH_NAME
    git push origin :$OLD_BRANCH_NAME
    git branch --unset-upstream $NEW_BRANCH_NAME
    git push origin $NEW_BRANCH_NAME
    git push origin -u $NEW_BRANCH_NAME
}

extract() {
    local c e i
    (($#)) || return
    for i; do
        c=''
        e=1
        if [[ ! -r $i ]]; then
            echo "$0: file is unreadable: \`$i'" >&2
            continue
        fi
        case $i in
        *.t@(gz|lz|xz|b@(2|z?(2))|a@(z|r?(.@(Z|bz?(2)|gz|lzma|xz))))) c='bsdtar xvf' ;;
        *.7z) c='7z x' ;;
        *.Z) c='uncompress' ;;
        *.bz2) c='bunzip2' ;;
        *.exe) c='cabextract' ;;
        *.tar.gz) c='tar -xf' ;;
        *.gz) c='gunzip' ;;
        *.rar) c='unrar x' ;;
        *.xz) c='unxz' ;;
        *.zip) c='unzip' ;;
        *) echo "$0: unrecognized file extension: \`$i'" >&2; continue ;;
        esac
        command $c "$i"
        e=$?
    done
    return $e
}

# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
    . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi

# Visual Studio Dev Environment Helpers
export _ORIGINAL_PATH="$PATH"
export VCVARS_BASH="$HOME/AppData/Roaming/git/vcvars-bash-prerelease"

if [ -d "$VCVARS_BASH" ]; then
    path_prepend_unique "$VCVARS_BASH"
fi

vc_on() {
    local arch="${1:-amd64}"
    if [ -z "$VCVARS_BASH" ]; then
        echo "VCVARS_BASH not set" >&2
        return 1
    fi
    local script="$VCVARS_BASH/vcvarsall.sh"
    if [ ! -f "$script" ]; then
        echo "vc_on: missing $script" >&2
        return 1
    fi
    echo "Switching to VS Dev environment for arch: $arch..."
    eval "$(sh "$script" "$arch")"
}

vc_off() {
    if [[ -n "$_PRE_VCPATH" ]]; then
        export PATH="$_PRE_VCPATH"
        unset _PRE_VCPATH
        echo "Reverted PATH to pre-VC state."
    else
        echo "No previous VC environment saved."
    fi
}

alias vc_enable='vc_on'
alias vc_disable='vc_off'

vcvars64() {
    local arch="${1:-amd64}"
    export VCPKG_ROOT="/c/src/vcpkg"
    export VCPKGRS_TRIPLET="x64-windows-static"
    local script="${VCVARS_BASH:-${HOME}/git/vcvars-bash-prerelease}/vcvarsall.sh"
    if [ ! -f "$script" ]; then
        echo "vcvars64: missing $script" >&2
        return 1
    fi
    eval "$(sh "$script" "$arch")"
}

# Cross-platform file explorer shortcut
e() {
    local target="${1:-.}"
    local uname_out
    uname_out="$(uname -s)"
    case "$uname_out" in
        Darwin*)
            open "$target"
            ;;
        Linux*)
            if grep -qi microsoft /proc/version 2>/dev/null && command -v explorer.exe >/dev/null 2>&1; then
                if command -v wslpath >/dev/null 2>&1; then
                    explorer.exe "$(wslpath -w "$target")"
                else
                    explorer.exe "$target"
                fi
            elif command -v thunar >/dev/null 2>&1; then
                thunar "$target" >/dev/null 2>&1 & disown
            elif command -v xdg-open >/dev/null 2>&1; then
                xdg-open "$target" >/dev/null 2>&1 & disown
            else
                echo "e: no graphical file manager found" >&2
                return 1
            fi
            ;;
        MSYS*|MINGW*|CYGWIN*)
            if command -v cygpath >/dev/null 2>&1; then
                explorer.exe "$(cygpath -w "$target")"
            else
                explorer.exe "$target"
            fi
            ;;
        *)
            if command -v explorer.exe >/dev/null 2>&1; then
                explorer.exe "$target"
            else
                echo "e: unsupported platform" >&2
                return 1
            fi
            ;;
    esac
}

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/mkusper/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/mkusper/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/mkusper/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/mkusper/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

alias docker-compose="docker compose"

# Easy-to-type copy and paste for X11
if [[ "$XDG_SESSION_TYPE" == "x11" ]]; then
    alias copy='xclip -selection clipboard'
    alias paste='xclip -selection clipboard -o'
fi
export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"
