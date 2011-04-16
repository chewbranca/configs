#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export EDITOR=vim

alias ls='ls -lh --color=auto'
#PS1='[\u@\h \W]\$ '

PS1='\[\e[1;32m\][\u@\h \W]\$\[\e[0m\] '

alias xx='exit'
alias shutdown='shutdown -h now'

clyde() {
   case $1 in
       -S | -S[^sih]* | -R* | -U*)
           /usr/bin/sudo /usr/bin/clyde "$@" ;;
       *)
           /usr/bin/clyde "$@" ;;
   esac
}

alias shutdown!='sudo shutdown -h now'
