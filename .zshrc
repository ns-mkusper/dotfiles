# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Theme
ZSH_THEME="cypher"

plugins=(git branch git-prompt)

source $ZSH/oh-my-zsh.sh

# Disable bracketed paste mode
unset zle_bracketed_paste

#ensure active python virtualenv is on the prompt
export VIRTUAL_ENV_DISABLE_PROMPT=0

function virtualenv_info {
    [ $VIRTUAL_ENV ] && echo '('$(basename $VIRTUAL_ENV)') '
}

# cypher theme prompt customization
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

# history settings
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

# ensure jump-by-word works as expected
export WORDCHARS="/\\\()\"'-.,:;<>~\!@#$%^&*|+=[]{}~?|"

# bash my aws
if [ -d ${HOME}/.bash-my-aws ]; then
    for f in ${HOME}/.bash-my-aws/lib/*-functions; do source $f; done
fi

export PATH="/usr/local/bin/:${PATH}:${HOME}/bin/:/usr/sbin/:${HOME}/go/bin:${HOME}/.local/bin"

#enable rust cargo binaries
export PATH="${HOME}/.cargo/bin:${PATH}"

# OS-specific stuff
unameOut="$(uname -s)"
case "${unameOut}" in
Linux*)
    machine=Linux
    export PYENV_ROOT="$HOME/.pyenv"
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
    export MINGW_ORIGINAL_PATH=$PATH

    function mingw_clear_external_build_path() {
        export PATH=$(echo ${PATH} | awk -v RS=: -v ORS=: '/c\// {next} {print}' | sed 's/:*$//')
    }

    export VCPKG_ROOT=~/git/vcpkg

    SAFE_START_DIR=$(cygpath "$STARTDIR")
    if [ -d "$SAFE_START_DIR" ]; then cd "$SAFE_START_DIR"; fi

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
    ;;
*)
    machine="UNKNOWN:${unameOut}"
    ;;
esac

# pyenv setup
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
# End Nix

# Quick VS environment setup matching "x64 Native Tools Command Prompt for VS 2022".
vcvars64() {
    local arch="${1:-amd64}"
    export VCPKG_ROOT="/c/src/vcpkg"
    export VCPKGRS_TRIPLET="x64-windows-static"
    local script="${HOME}/git/vcvars-bash-prerelease/vcvarsall.sh"
    if [ ! -f "$script" ]; then
        echo "vcvars64: missing $script" >&2
        return 1
    fi
    eval "$(sh "$script" "$arch")"
}

# Cross-platform file explorer shortcut; default to current directory.
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

