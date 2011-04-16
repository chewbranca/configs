# ~/.xmonad/autostart.sh
#!/bin/sh

# Programs to launch at startup
#xsetroot -cursor_name pCircle-24
#sh ~/.fehbg &

# Programs which will run after Xmonad has started
(sleep 2 && chromium) &
(sleep 2 && pacmd set-default-sink combined) &
(sleep 2 && pidgin) &
(sleep 2 && urxvt) &
(sleep 2 && stalonetray) &
(sleep 2 && dropboxd) &
