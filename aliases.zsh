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
alias val="v ~/.oh-my-zsh/custom/aliases.zsh"
alias vv="v $NVIM_CONFIG/init.vim"

# qol
alias sf=screenfetch
alias duh="du -sh"
alias v=nvim
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

# echo évenement | aspell -a
spell() {
  echo $1 | aspell -a;
}
