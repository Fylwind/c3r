#!/bin/sh
#
# be sure to start the VM first with `VBoxHeadless -s <name>`

vm=vm-deb
srv=tails
app=c3r

# ----------------------------------------------------------------------------

set -e

# shell-escape the given variable
#
# inputs:
#   - 1: name of the variable
#
# output:
#   - the escaped string is stored in a variable of the same name
#     but suffixed with an underscore
#
escape() {
    eval '_1=$'"$1"
    _2="s/'/'\\\\''/g"
    _1=\'`printf "%s" "$_1" | sed "$_2"`\'
    eval "$1"'_=$_1'
}

# apt-get install libsqlite3-dev
# git clone --recursive https://github.com/Fylwind/c3r

scp Keys.hs "$vm:$app/Keys.hs"
escape app
ssh -T "$vm" <<EOF
set -e
cd $app_
git fetch -p origin
git reset --hard origin/master
git submodule update
cabal sandbox init
cabal install --dependencies-only
cabal build --ghc-options=-optl-static
EOF
echo 'Built.'
rsync -aPvz "$vm:$app/dist/build/$app/$app" "/tmp/$app"
rsync -aPvz "/tmp/$app" "$srv:/usr/local/bin/$app"
echo 'Copied executable.'
ssh -T "$srv" pkill -x "$app"
echo 'Restarted daemon.'
