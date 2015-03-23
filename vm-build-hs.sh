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
    _1="s/'/'\\\\''/g"
    eval '_2=$'"$1"
    _1=`printf "'"; printf "%s" "$_2" | sed "$_1"; printf "'"`
    eval "$1"'_=$_1'
}

escape app
ssh "$vm" <<EOF
cd $app_
git fetch -p origin
git reset --hard origin/master
cabal build --ghc-options=-optl-static
EOF
rsync -aPvz "$vm:$app/dist/build/$app/$app" "/tmp/$app"
rsync -aPvz "/tmp/$app" "$srv:/usr/local/bin/$app"
ssh "$srv" pkill -x "$app"
