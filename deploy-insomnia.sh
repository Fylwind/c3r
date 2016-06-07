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

sudo install -m755 "\`stack exec which $app\`" /usr/local/bin/$app
sudo tee /etc/systemd/system/$app.service >/dev/null <<SERVICEEOF
[Unit]
After=network.target

[Service]
Type=simple
Restart=always
RestartSec=30
ExecStart=/usr/local/bin/$app
User=$app
WorkingDirectory=/var/lib/$app

[Install]
WantedBy=default.target
SERVICEEOF

groups $app >/dev/null 2>&1 ||
sudo useradd -r -s /usr/sbin/nologin -m -d /var/lib/$app $app
sudo chmod 700 /var/lib/$app

sudo systemctl daemon-reload
sudo systemctl enable $app
if sudo [ -f /var/lib/$app/$app.db ]; then
    sudo systemctl restart $app
else
    printf '%s' 'Note: Service is not yet configured; run:

    (cd /var/lib/$app && sudo -u $app /usr/local/bin/$app)

to configure.  Then, start the service using \`systemctl start $app\`.'
fi
EOF
echo Done.
