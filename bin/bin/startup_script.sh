#!/bin/zsh

# shadowsocks
/home/yhxie/Dropbox/software/script/ss.sh

# dropbox
dropbox-cli start

# unclutter
unclutter --timeout 5 &

# start xkeysnail
sudo /usr/bin/xkeysnail --watch -q /home/yhxie/.config/xkeysnail/xkeysnail-config.py >/dev/null 2>&1 &

# start gsd-xsettings which is needed TIM and wechat
/usr/lib/gsd-xsettings &

V2RAY_LOCATION_ASSET=/etc/v2ray v2ray --config /etc/v2ray/config-1.json &
