#set -x
echo "Sourcing ${HOME}/.bashrc..."


# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
    # Shell is non-interactive.  Be done now!
    return
fi

# default prompt
PS1='[\u@\h \W]\$ '
#PS1="\[${COLOR_LIGHT_CYAN}\]\u\[${COLOR_WHITE}\]@\[${COLOR_LIGHT_PURPLE}\]\H \[${COLOR_WHITE}\]\w \[${COLOR_CYAN}\]\$ \[${COLOR_NC}\]"  # Primary prompt with only a path
#GPG NEEDS THIS TO AVOID gpg: signing failed: Inappropriate ioctl for device
export GPG_TTY=$(tty)


# Bash won't get SIGWINCH if another process is in the foreground.
# Enable checkwinsize so that bash will check the terminal size when
# it regains control.  #65623
# http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
#shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# {{{ History
# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
export HISTCONTROL=ignoredups:erasedups
shopt -s histappend
export PROMPT_COMMAND="history -a;$PROMPT_COMMAND"
export HISTFILESIZE=100000000
export HISTSIZE=100000000
shopt -s cmdhist
shopt -s dotglob
shopt -s extglob
set show-all-if-ambiguous on
set bell-style visible


# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
# }}}

# ssh tab complete hostnames that you have known_hosts entries for.
if [ -r ~/.ssh/known_hosts ] || [ -r /etc/ssh/ssh_known_hosts ];then
    complete -W "$(awk '{print $1}' ~/.ssh/known_hosts | awk -F, '{print $1}' | sort | uniq | grep -v "\[")" ssh
fi

[[ -f /etc/profile.d/bash-completion ]] && source /etc/profile.d/bash-completion

complete -cf sudo
complete -cf man
export GIT_EDITOR="emacs"
export EDITOR="emacs"
export BROWSER="firefox"
#TERM=xterm-256color
export PATH="${PATH}:/usr/local/bin/:/home/$(whoami)/bin/:/usr/sbin/:~/go/bin"
if [[ "$OSTYPE" == "darwin"* ]]; then
    export PATH="${PATH}:~/Library/Python/3.6/bin:~/.telegram-cli/bin:/Users/$(whoami)/Library/Python/2.7/bin:/usr/local/Cellar/poppler/0.69.0_1/bin"
fi
export OSFONTDIR="/usr/share/fonts;$HOME/fonts"
export TEXMFLOCAL="/home/$(whoami)/.texlive"
export TEXMFHOME="/home/$(whoami)/.texlive"


txtblk='\e[0;30m' # Black - Regular
txtred='\e[0;31m' # Red
txtgrn='\e[0;32m' # Green
txtylw='\e[0;33m' # Yellow
txtblu='\e[0;34m' # Blue
txtpur='\e[0;35m' # Purple
txtcyn='\e[0;36m' # Cyan
txtwht='\e[0;37m' # White
bldblk='\e[1;30m' # Black - Bold
bldred='\e[1;31m' # Red
bldgrn='\e[1;32m' # Green
bldylw='\e[1;33m' # Yellow
bldblu='\e[1;34m' # Blue
bldpur='\e[1;35m' # Purple
bldcyn='\e[1;36m' # Cyan
bldwht='\e[1;37m' # White
unkblk='\e[4;30m' # Black - Underline
undred='\e[4;31m' # Red
undgrn='\e[4;32m' # Green
undylw='\e[4;33m' # Yellow
undblu='\e[4;34m' # Blue
undpur='\e[4;35m' # Purple
undcyn='\e[4;36m' # Cyan
undwht='\e[4;37m' # White
bakblk='\e[40m'   # Black - Background
bakred='\e[41m'   # Red
bakgrn='\e[42m'   # Green
bakylw='\e[43m'   # Yellow
bakblu='\e[44m'   # Blue
bakpur='\e[45m'   # Purple
bakcyn='\e[46m'   # Cyan
bakwht='\e[47m'   # White
txtrst='\e[0m'    # Text Reset


# Set colorful PS1 only on colorful terminals.
# dircolors --print-database uses its own built-in database
# instead of using /etc/DIR_COLORS.  Try to use the external file
# first to take advantage of user additions.  Use internal bash
# globbing instead of external grep binary.

# Dynamically modified variables. Do not change them!
use_color=true

source ~/.git-prompt.sh
for f in ~/.bash-my-aws/lib/*-functions; do . ${f}; done


# sanitize TERM:
safe_term=${TERM//[^[:alnum:]]/?}
match_lhs=""

[[ -f ~/.dir_colors   ]] && match_lhs="${match_lhs}$(<~/.dir_colors)"
[[ -f /etc/DIR_COLORS ]] && match_lhs="${match_lhs}$(</etc/DIR_COLORS)"
[[ -z ${match_lhs}    ]] \
    && type -P dircolors >/dev/null \
    && match_lhs=$(dircolors --print-database)
[[ $'\n'${match_lhs} == *$'\n'"TERM "${safe_term}* ]] && use_color=true

if ${use_color} ; then
    # Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
    if type -P dircolors >/dev/null ; then
        if [[ -f ~/.dir_colors ]] ; then
            eval $(dircolors -b ~/.dir_colors)
        elif [[ -f /etc/DIR_COLORS ]] ; then
            eval $(dircolors -b /etc/DIR_COLORS)
        fi

    fi

    if [[ ${EUID} == 0 ]] ; then
        PS1="\[${txtwht}\]@\H \w\$(__git_ps1) \[${txtred}\]\$ \[${txtrst}\]"  # Primary prompt with only a path
    else
        PS1="\[${txtcyn}\]\u\[${txtwht}\]@\[${txtwht}\]\H \[${txtwht}\]\w\$(__git_ps1) \[${txtcyn}\]\$ \[${txtrst}\]"  # Primary prompt with only a path
    fi

    # Add handy color-enabled aliases
    alias tg='~/github/tg/bin/telegram-cli'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
else
    if [[ ${EUID} == 0 ]] ; then
        # show root@ when we do not have colors
        PS1='\u@\h \W \$ '
    else
        PS1='\u@\h \W \$ '
    fi
fi

PS2='> '
PS3='> '
PS4='+ '

# }}}


# {{{ Aliases
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi
# }}}


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



 # {{{ Cleanup
 # Try to keep environment pollution down, EPA loves us.
 unset use_color safe_term match_lhs
 # }}}

 sss() {
     if [ ! -z $1 ];then
         re='^i-[0-9A-Za-z]+'
         if [[ $1 =~ $re ]];then
             INSTANCE_ID=$1
         else
             INSTANCE_ID=$(aws cloudformation describe-stack-resources --stack-name=$1 | grep -oE 'i-[a-zA-Z0-9]{1,}' | head -n 1)
         fi

         echo $INSTANCE_ID
         INSTANCE_PUBLIC_DNS=$(aws ec2 describe-instances --query 'Reservations[].Instances[][PublicDnsName]' --filters "Name=instance-id,Values=${INSTANCE_ID}" --output text)

         ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -A -t $(whoami)@${BASTION} ssh -A -t  centos@${INSTANCE_PUBLIC_DNS}
         if [ $? -gt 0 ];then
             ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -A -t $(whoami)@${BASTION} ssh -A -t  ec2-user@${INSTANCE_PUBLIC_DNS}
         fi
     else
         echo "BAD INPUT!"
     fi
 }



for f in ~/.bash-my-aws/lib/*-functions; do source $f; done
source ~/.bash-my-aws/bash_completion.sh

export GOPATH=$HOME/go


compress ()
{
    [[ "${1}" =~ ^\..*$ ]] && 1=$(sed 's/^..//' <<< "${1}");
    shortname=$(basename "${1}" |awk -F'.' '{print $1}');
    XZ_OPT=-9 tar -Jcvf "${shortname}.tar.xz" "${1}"
}

#enable rust cargo binaries
export PATH="${HOME}/.cargo/bin:${PATH}"

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
 eval "$(pyenv init -)"
fi
