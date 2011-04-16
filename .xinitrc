#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
setxkbmap -option terminate:ctrl_alt_bksp
xsetroot -cursor_name left_ptr
nitrogen --restore &
#conky | while read -r; do xsetroot -name "$REPLY"; done &
#
#while true
#do
#	xsetroot -name  "$(date +"%a %b %d %r")"
#	sleep 1s
#done &
pulseaudio --kill && pulseaudio --start &
mpd ~/.mpd/mpd.conf
g15mpd &
#exec dwm
#exec awesome
xmonad
