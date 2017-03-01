stty -ixon

bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

RPROMPT=''
fpath=( "/home/seanstrom/.zsh/zfunctions" $fpath )
source ~/.zsh/antigen-hs/init.zsh

# chruby stuff
source /usr/local/opt/chruby/share/chruby/chruby.sh
RUBIES+=(~/.rubies/*)

# autocompletion stuff
autoload -Uz compinit && compinit

# eval `dircolors ~/.dir_colors/dircolors.ansi-universal`
alias emc='emacsclient -t'
alias emg='emacsclient -c'
