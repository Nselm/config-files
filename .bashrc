alias ls='ls -h --color=auto'
alias grep='grep -E --color'
alias copy='xclip -sel clip'
alias out='xclip -o'
alias CB='xbacklight -set'

alias fl="-g -Wall"

PS1=$(echo "ORANGE|BOLD \w > CLR|GREEN|BOLD" | awk -f ~/bin/colors.awk)
export PATH=$PATH:~/bin:~/.local/bin
export EDITOR="vim"
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c -a emacs"













