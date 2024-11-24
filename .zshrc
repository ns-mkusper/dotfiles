# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="cypher"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git branch git-prompt)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Disable bracketed paste mode
unset zle_bracketed_paste

#ensure active python virtualenv is on the prompt
export VIRTUAL_ENV_DISABLE_PROMPT=0

function virtualenv_info {
    [ $VIRTUAL_ENV ] && echo '('$(basename $VIRTUAL_ENV)') '
}

# cypher theme doesn't show git prompt by default
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
# set history size
export HISTSIZE=999999999
# save history size
export SAVEHIST=999999999
#history file
export HISTFILE=${HOME}/.zsh_history
# incrementally append to history file after each command
setopt APPEND_HISTORY INC_APPEND_HISTORY
setopt BANG_HIST              # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY       # Write the history file in the ":start:elapsed;command" format.
setopt INC_APPEND_HISTORY     # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY          # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS       # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS   # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS      # Do not display a line previously found.
setopt HIST_IGNORE_SPACE      # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS      # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS     # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY            # Don't execute immediately upon history expansion.

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
# work
Darwin*)
    export PYENV_ROOT="$HOME/.pyenv"
    # enable gnu coreutils
    export PATH="/usr/local/opt/coreutils/bin/:${PATH}"
    # enable brew apps
    export PATH="/opt/homebrew/bin/:${PATH}"
    # enable tex (installed by brew)
    export PATH="/usr/local/opt/texinfo/bin/:${PATH}"

    export NVM_DIR="$HOME/.nvm"
    [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"                                       # This loads nvm
    [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" # This loads nvm bash_completion

    NETSKOPE_CERT_BUNDLE="/opt/netskope-cert-bundle.pem"
    REQUESTS_CA_BUNDLE="/opt/netskope-cert-bundle.pem"
    export AWS_CA_BUNDLE="/opt/netskope-cert-bundle.pem"
    machine=Mac
    ;;
CYGWIN*)
    machine=Cygwin
    ;;
MSYS_NT*)
    machine=MSYS2
    export MSYS=winsymlinks:native # closest behavior to linux symlinks
    export PYENV_ROOT="/c/Users/mkusp/.pyenv/pyenv-win"
    # For building with MINGW it's helpful to switch between MSVC, MINGW, etc. build tools which normally means altering the PATH order
    # TODO: create more functions to alter path order depending on build type
    export MINGW_ORIGINAL_PATH=$PATH
    # when building with MinGw outside paths can pollute the local build environment
    # TODO: add ones for MSVC versions and other build scenarios
    function mingw_clear_external_build_path() {
        export PATH=$(echo ${PATH} | awk -v RS=: -v ORS=: '/c\// {next} {print}' | sed 's/:*$//')
    }
    export VCPKG_ROOT=~/git/vcpkg

    # ensure emacs and other apps using msys open bash prompts in the correct place
    SAFE_START_DIR=$(cygpath "$STARTDIR")
    if [ -d "$SAFE_START_DIR" ]; then cd "$SAFE_STARTDIR"; fi
    ;;
    *)
    machine="UNKNOWN:${unameOut}"
    ;;
esac

# windows pyenv doesn't have init and virtualenv
if which pyenv; then
    # pyenv
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
    # Rename the local branch to the new name
    git branch -m $OLD_BRANCH_NAME $NEW_BRANCH_NAME

    # Delete the old branch on remote - where origin is, for example, origin
    git push origin --delete $OLD_BRANCH_NAME

    # Or shorter way to delete remote branch [:]
    git push origin :$OLD_BRANCH_NAME

    # Prevent git from using the old name when pushing in the next step.
    # Otherwise, git will use the old upstream name instead of $NEW_BRANCH_NAME.
    git branch --unset-upstream $NEW_BRANCH_NAME

    # Push the new branch to remote
    git push origin $NEW_BRANCH_NAME

    # Reset the upstream branch for the new_name local branch
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
        *.t@(gz|lz|xz|b@(2|z?(2))|a@(z|r?(.@(Z|bz?(2)|gz|lzma|xz)))))
            c='bsdtar xvf'
            ;;
        *.7z) c='7z x' ;;
        *.Z) c='uncompress' ;;
        *.bz2) c='bunzip2' ;;
        *.exe) c='cabextract' ;;
        *.tar.gz) c='tar -xf' ;;
        *.gz) c='gunzip' ;;
        *.rar) c='unrar x' ;;
        *.xz) c='unxz' ;;
        *.zip) c='unzip' ;;
        *)
            echo "$0: unrecognized file extension: \`$i'" >&2
            continue
            ;;
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
