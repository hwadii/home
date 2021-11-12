export MOZ_DBUS_REMOTE=1
export MOZ_WEBRENDER=1
export PATH="$PATH:$HOME/.local/bin:$HOME/.local/cargo/bin"

setxkbmap us -option caps:escape
exec i3
