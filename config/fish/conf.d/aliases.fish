# verbose
alias rm "rm -v"
alias cp "cp -v"
alias mv "mv -v"

alias sard "mpv https://twitch.tv/sardoche --quiet >/dev/null"
alias kameto "mpv https://www.twitch.tv/kamet0 --quiet >/dev/null"

alias dragon-x11 "GDK_BACKEND=x11 dragon"

# list
alias ls "eza --git --group-directories-first --hyperlink"
alias l ls
alias ll "ls -l"
alias lll "ls -l --icons -h"
alias la "ls -l -a -I.DS_Store"
alias lh "ls -l -a -H -I.DS_Store"
alias lm "ls -l -s=modified"
alias lc "ls --created -s=created"
alias lt "ls --tree --git-ignore"
alias lr "ls -R --git-ignore"

# shortcuts to edit various files
alias vim nvim
alias vi nvim

# qol
alias wholistens "netstat -tulpn | rg LISTEN"
alias p python3
alias map "xargs -n1"
alias md "mkdir -p"

alias mp3 "youtube-dl -x --embed-thumbnail --audio-format mp3"

alias ytdl yt-dlp

# more git aliases
alias gin "git init"
alias gcd "git checkout dev"
alias gdf "git diff FETCH_HEAD"
alias gcnv! "git commit --no-verify"
alias gme "git machete"
alias gmes "git machete status --list-commits"
alias gmet "git machete traverse"
alias gmetw "git machete traverse -W"
alias gmeu "git machete update"

# lazygit
alias lg lazygit

alias dc "docker compose"

# tmux
abbr ta "tmux attach -t"
abbr tad "tmux attach -d -t"
abbr tkss "tmux kill-session -t"
abbr tksv "tmux kill-server"
abbr tl "tmux list-sessions"
abbr ts "tmux new-session -s"

# extract
alias x "aunpack $argv"

# bang
alias b bangdo
