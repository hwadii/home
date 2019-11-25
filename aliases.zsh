NVIM_CONFIG="/home/hwadii/.config/nvim"
# verbose
alias rm="rm -v"
alias cp="cp -v"
alias mv="mv -v"

alias sard="mpv https://twitch.tv/sardoche"
alias kameto="mpv https://www.twitch.tv/kamet0"
alias open="xdg-open"

# list
alias l="exa --git --group-directories-first"
alias ll="l -l"
alias la="ll -a"
alias lk="ll -s=size"
alias lm="ll -s=modified"
alias lc="ll --created -s=created"

# shortcuts to edit various files
alias ezsh="e ~/.zshrc"
alias esc="e ~/.oh-my-zsh/custom/scripts.sh"
alias rzsh=". ~/.zshrc"
alias val="e ~/.oh-my-zsh/custom/aliases.zsh"
alias ev="e $NVIM_CONFIG/init.vim"

# qol
alias wholistens="netstat -tulpn | rg LISTEN"
alias gdf="git diff FETCH_HEAD"
alias sf=screenfetch
alias duh="du -sh"
alias e=nvim
alias p=python3
alias xco="cd ~/code"
alias xan="cd ~/anime"
alias xdl="cd ~/Téléchargements"
alias xds="cd ~/Bureau"
alias xpl="cd ~/code/playing-around"
alias xwk="cd ~/work"

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
alias nrb="npm run build"
alias nrt="npm run test"
alias nrc="npm run commit"

# toolbox
alias tb=toolbox
alias tbc="tb create"
alias tbe="tb enter"
alias tbls="tb list"
alias tbrm="tb rm"

# echo évenement | aspell -a
spell() {
  echo $1 | aspell -a;
}
alias youtube-dl="youtube-dl -f 'mp4[height>=?720]'"
