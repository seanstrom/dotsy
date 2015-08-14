#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

# added by Nix installer
if [ -e /home/seanstrom/.nix-profile/etc/profile.d/nix.sh ]; then . /home/seanstrom/.nix-profile/etc/profile.d/nix.sh; fi
