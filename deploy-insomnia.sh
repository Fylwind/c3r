#!/bin/sh
#
# Initial preparation
# -------------------
#
#   <install Stack using instructions from official site>
#
#   apt-get install -y libsqlite3-dev
#
set -eu

srv=insomnia
app=c3r
bindir=/usr/local/bin
remote=https://github.com/Fylwind/$app
branch=${1-master}

# ----------------------------------------------------------------------------

rsync -aPvc Keys.hs "$srv:$app/Keys.hs"
ssh -T "$srv" <<EOF
set -eu
[ -d $app ] ||
    git clone --recursive $remote $app
cd $app
git fetch -p origin
git reset --hard origin/$branch
git submodule update --init --recursive
stack init --force
stack build
sudo install -m755 "\`stack exec which $app\`" $bindir/$app
sudo pkill -x $app
EOF
echo Done.
