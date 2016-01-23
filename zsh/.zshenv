export PATH=~/.npm-global/bin:$PATH:~/.nix-profile/bin
export NIXPKGS=~/dev/nixpkgs
export EDITOR=vim

NIX_GHC_BIN=$(dirname `which ghc`)

export NIX_GHC="$NIX_GHC_BIN/ghc"
NIX_GHC_VERSION=$($NIX_GHC --numeric-version)
export NIX_GHCPKG="$NIX_GHC_BIN/ghc-pkg"
export NIX_GHC_DOCDIR="$NIX_GHC_BIN/../share/doc/ghc/html"
export NIX_GHC_LIBDIR="$NIX_GHC_BIN/../lib/ghc-${NIX_GHC_VERSION}"
