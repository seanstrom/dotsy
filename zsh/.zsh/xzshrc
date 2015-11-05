# Set PATH
export PATH=$(~/.dotmods/bin/get-paths | tr '\n' ':')

# Set aliases
while read LINE; do eval "alias $LINE"; done < <(get-aliases =)

# Set PS1 to ~/.config/bin/echo-ps1
precmd () { export PS1="$(~/.dotmods/bin/echo-ps1 $?)" }

npm_chpwd_hook() {
    if [ -n "${PRENPMPATH+x}" ]; then
        PATH=$PRENPMPATH
        unset PRENPMPATH
    fi
    if [ -f package.json ]; then
        PRENPMPATH=$PATH
        PATH=$(npm bin):$PATH
    fi
}
 
add-zsh-hook preexec npm_chpwd_hook

source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
