# verbose
alias rm="rm -v"
alias cp="cp -v"
alias mv="mv -v"
alias -s txt=cat

tw() {
  mpv https://twitch.tv/"$@" --quiet >/dev/null &
}

vcsv() {
  perl -pe 's/((?<=,)|(?<=^)),/ ,/g;' | column -t -s, | less  -F -S -X -K
}
alias sard="mpv https://twitch.tv/sardoche --quiet >/dev/null &"
alias kameto="mpv https://www.twitch.tv/kamet0 --quiet >/dev/null &"
alias coro="mpv https://twitch.tv/corobizar --quiet >/dev/null &"
alias open="open_command"

# list
alias ls="exa --git --group-directories-first"
alias l=ls
alias ll="exa --git --group-directories-first -l"
alias la="exa --git --group-directories-first -l -a"
alias lk="exa --git --group-directories-first -l -s=size"
alias lm="exa --git --group-directories-first -l -s=modified"
alias lc="exa --git --group-directories-first --created -s=created"
alias lT="exa --git --group-directories-first --tree --git-ignore"
alias lR="exa --git --group-directories-first -R --git-ignore"

# shortcuts to edit various files
alias vi=nvim
alias vim=nvim
alias ezsh="nvim ~/.zshrc"
alias esc="nvim ~/.oh-my-zsh/custom/scripts.sh"
alias rzsh=". ~/.zshrc"
alias val="nvim ~/.oh-my-zsh/custom/aliases.zsh"
alias ev="nvim ~/.config/nvim/init.vim"

# qol
alias wholistens="netstat -tulpn | rg LISTEN"
alias gdf="git diff FETCH_HEAD"
alias sf=screenfetch
alias duh="du -sh"
alias p=python3
alias xco="cd ~/code"
alias xan="cd ~/anime"
alias xdl="cd ~/Téléchargements"
alias xcr="cd ~/cruft"
alias xpl="cd ~/code/playing-around"
alias xwk="cd ~/work"
# Trim new lines and copy to clipboard
alias c="tr -d '\n' | clipcopy"
# Intuitive map function
# # For example, to list all directories that contain a certain file:
# # find . -name .gitattributes | map dirname
alias map="xargs -n1"

# tar
alias tarx="tar xvf"
alias targz="tar xvfz"
alias tarc="tar cvfz"

# npm
alias ni="npm install"
alias nin="npm init"
alias nu="npm update"
alias nis="npm i -S"
alias nid="npm i -D"
alias nig="npm i -g"
alias nr="npm run"
alias nrs="npm run start"
alias nrss="npm run start:staging"
alias nrb="npm run build"
alias nrt="npm run test"
alias nrc="npm run commit"

# toolbox
alias tb=toolbox
alias tbc="tb create"
alias tbe="tb enter"
alias tbls="tb list"
alias tbrm="tb rm"

alias mp3="youtube-dl -x --embed-thumbnail --audio-format mp3"
alias pyformat="autopep8 --in-place"

alias dc=docker-compose

function blob() {
  echo "$@" | sed "s|blob://|s3://|g" | xargs s3cmd -c ~/.s3cfg-blob | sed "s|s3://|blob://|g" | sed "s|s3cmd |blob |g";
}
alias ytdl=youtube-dl

# more git aliases
alias gin="git init"
alias gcd="git checkout dev"
alias e2e="docker-compose -f ~/code/cardiologs/front/cypress/docker-compose.yml"
alias spotify="LD_PRELOAD=~/.local/lib/spotifywm.so /usr/share/spotify/spotify"
