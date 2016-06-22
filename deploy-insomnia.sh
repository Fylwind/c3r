#!/bin/sh
#
# Initial preparation
# -------------------
#
#   <install Stack using instructions from official site>
#
set -eu

srv=insomnia
app=c3r
remote=https://github.com/Fylwind/$app
branch=${1-master}

# ----------------------------------------------------------------------------

rsync -aPvc Keys.hs "$srv:$app/Keys.hs"
ssh -T "$srv" <<EOF
set -eu
changed=

sudo apt-get -y install libsqlite3-dev stack

[ -d $app ] ||
    git clone --recursive $remote $app
cd $app
git fetch -p origin
git reset --hard origin/$branch
git submodule update --init --recursive

stack upgrade
stack --resolver=lts-6.1 init --force
stack build

cmp "\`stack exec which $app\`" /usr/local/bin/$app >/dev/null 2>&1 || {
    sudo install -m755 "\`stack exec which $app\`" /usr/local/bin/$app
    changed=t
}

cmp $app.service /etc/systemd/system/$app.service >/dev/null 2>&1 || {
    sudo cp $app.service /etc/systemd/system/$app.service
    changed=t
}

groups $app >/dev/null 2>&1 ||
sudo useradd -r -s /usr/sbin/nologin -m -d /var/lib/$app $app
sudo chmod 700 /var/lib/$app
sudo cp -r tools /var/lib/$app

if [ "\$changed" ]; then
    sudo systemctl daemon-reload
    sudo systemctl enable $app
    if sudo [ -f /var/lib/$app/$app.db ]; then
        sudo systemctl restart $app
    else
        printf '%s' 'Note: Service is not yet configured; to configure, run:

    (cd /var/lib/$app && sudo -u $app /usr/local/bin/$app)
    sudo systemctl start $app

'
    fi
fi
script -e -q -c "sudo systemctl status --no-pager $app.service" /dev/null
EOF
