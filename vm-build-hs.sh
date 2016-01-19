#!/bin/sh
#
# Initial preparation
# -------------------
#
#   apt-get install -y libsqlite3-dev
#
# Build procedure
# ---------------
#
# 1. Start the virtual machine: VBoxHeadless -s <vm>
# 2. Log into a shared SSH session with the server: ssh <srv>
# 3. Run: ./vm-build-hs.sh [<branch>]
#
set -eu

vm=vm-deb
srv=tails
app=c3r
bindir=.local/bin
remote=https://github.com/Fylwind/$app
branch=${1-master}

# ----------------------------------------------------------------------------

scp Keys.hs "$vm:$app/Keys.hs"
ssh -T "$vm" <<EOF
set -eu
[ -d $app ] ||
    git clone --recursive $remote $app
cd $app
git fetch -p origin
git reset --hard origin/$branch
git submodule update --init --recursive
./build.sh --ghc-options=-optl-static
EOF
echo 'Build complete.'

rsync -az "$vm:$app/dist/build/$app/$app" "/tmp/$app"
rsync -aPz "/tmp/$app" "$srv:$bindir/$app"
echo 'Installed executable.'

if ssh -T "$srv" "pkill -x $app"
then echo 'Restarted daemon.'
else echo 'No daemon process found.'
fi
