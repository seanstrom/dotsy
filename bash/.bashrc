UNAME=`uname -s`

stty -ixon
bind -r '\C-s'

# Source all files in the ~/.dotmods/bash/autoload directory
for f in ~/.dotmods/bash/autoload/*; do source $f; done

# Set PATH
export PATH=$(~/.dotmods/bin/get-paths | tr '\n' ':')

# Set aliases
while read LINE; do eval "alias $LINE"; done < <(get-aliases =)

# Set PS1 to ~/.dotmods/bin/echo-ps1
export PROMPT_COMMAND="export PS1=\$(~/.dotmods/bin/echo-ps1 \$?)"

# function cd() { builtin cd "$@" && npm_chpwd_hook; }

# function npm_chpwd_hook() {
#   NPMPATH="$(npm bin)"
#
#   if [ -d "$NPMPATH" ]; then
#     PATH=$NPMPATH:$PRENPMPATH
#   else
#     PATH=$PRENPMPATH
#   fi
#
#   export PATH=$PATH
# }
#
# export PRENPMPATH=$PATH
# npm_chpwd_hook

if [ -e /home/seanstrom/.nix-profile/etc/profile.d/nix.sh ]; then . /home/seanstrom/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

# export PKG_CONFIG_PATH=/usr/local/opt/libffi/lib/pkgconfig

export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/local/lib
