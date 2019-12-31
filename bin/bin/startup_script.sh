#!/bin/zsh

# shadowsocks
/home/yhxie/Dropbox/software/script/ss.sh

# dropbox
dropbox-cli start

# unclutter
unclutter --timeout 5 &

# start xkeysnail
sudo /usr/bin/xkeysnail --watch -q /home/yhxie/.config/xkeysnail/xkeysnail-config.py >/dev/null 2>&1 &
