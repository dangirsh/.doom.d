#!/bin/bash

# Disable access control for the current user.
xhost +SI:localuser:$USER

# Identify the home of our gtkrc file, important for setting styles of
# gtk-based applications
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"


# Make Java applications aware this is a non-reparenting window manager.
export _JAVA_AWT_WM_NONREPARENTING=1

# Bind caps to ctrl
setxkbmap -option 'ctrl:nocaps'

# set keyboard rate
xset r rate 160 50

xsetroot -solid black

# Set default cursor.
xsetroot -cursor_name left_ptr

# Nix + direnv
# lorri daemon &

# Email sync
offlineimap &

# Uncomment the following block to use the exwm-xim module.
# export XMODIFIERS=@im=exwm-xim
# export GTK_IM_MODULE=xim
# export QT_IM_MODULE=xim
# export CLUTTER_IM_MODULE=xim

source ~/.profile

# Sync Doom
# ~/.emacs.d/bin/doom sync

# Finally start Emacs
exec ~/.emacs.d/bin/doom run
