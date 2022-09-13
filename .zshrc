# Enable Powerlevel10k instant prompt. Should stay close to the top of ${HOME}/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

bindkey "^R" history-incremental-search-backward
bindkey "\e[A" history-beginning-search-backward
bindkey "\e[B" history-beginning-search-forward
bindkey '\ef' emacs-forward-word
bindkey '\eb' emacs-backward-word


# history settings
#set history size
export HISTSIZE=999999999
#save history after logout
export SAVEHIST=999999999
#history file
export HISTFILE=${HOME}/.zsh_history
setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY          # Write the history file in the ":start:elapsed;command" format.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a line previously found.
setopt HIST_IGNORE_SPACE         # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS         # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY               # Don't execute immediately upon history expansion.


# To customize prompt, run `p10k configure` or edit ${HOME}/.p10k.zsh.
[[ ! -f ${HOME}/.p10k.zsh ]] || source ${HOME}/.p10k.zsh



#set -x
export GIT_EDITOR="emacs"
export EDITOR="emacs"

# ensure jump-by-word works as expected
export WORDCHARS='*?[]~&;!#$%^(){}<>'

# bash my aws
if [ -d ${HOME}/.bash-my-aws ]; then
  for f in ${HOME}/.bash-my-aws/lib/*-functions; do source $f; done
fi

export PATH="/usr/local/bin/:${PATH}:${HOME}/bin/:/usr/sbin/:${HOME}/go/bin:${HOME}/.local/bin"

#enable rust cargo binaries
export PATH="${HOME}/.cargo/bin:${PATH}"

case `uname` in
  Darwin)
      # enable gnu coreutils
      export PATH="/usr/local/opt/coreutils/bin/:${PATH}"
      # enable brew apps
      export PATH="/opt/homebrew/bin/:${PATH}"
      # enable tex (installed by brew)
      export PATH="/usr/local/opt/texinfo/bin/:${PATH}"
  ;;
  Linux)
  ;;
  FreeBSD)
  ;;
esac





git-rename(){
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
c='bsdtar xvf';;
*.7z)  c='7z x';;
*.Z)   c='uncompress';;
*.bz2) c='bunzip2';;
*.exe) c='cabextract';;
*.tar.gz)  c='tar -xf';;
*.gz)  c='gunzip';;
*.rar) c='unrar x';;
*.xz)  c='unxz';;
*.zip) c='unzip';;
*)     echo "$0: unrecognized file extension: \`$i'" >&2
       continue;;
esac

command $c "$i"
e=$?
done

return $e
}

export PATH=$(pyenv root)/libexec:$(pyenv root)/shims:$PATH
eval "$(pyenv init -)"
source /opt/homebrew/opt/powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
