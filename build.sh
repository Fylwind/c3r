#!/bin/sh
set -eu

[ -f cabal.sandbox.config ] ||
    cabal sandbox init

cabal exec -- sh -c "cd sqlite && cabal build && cabal copy"
cabal install --dependencies-only
cabal build "$@"
