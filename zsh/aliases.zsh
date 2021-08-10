# verbose
alias rm='rm -v'
alias cp='cp -v'
alias mv='mv -v'
alias -s txt=cat

tw() {
  mpv "https://twitch.tv/$@" --quiet > /dev/null & disown
}

alias sard='mpv https://twitch.tv/sardoche --quiet >/dev/null &'
alias kameto='mpv https://www.twitch.tv/kamet0 --quiet >/dev/null &'
alias coro='mpv https://twitch.tv/corobizar --quiet >/dev/null &'
alias open='open_command'

# list
alias ls='exa --git --group-directories-first'
alias l=ls
alias ll='exa --git --group-directories-first -l'
alias lll='exa --git --group-directories-first -l --icons -h'
alias la='exa --git --group-directories-first -l -a'
alias lk='exa --git --group-directories-first -l -s=size'
alias lm='exa --git --group-directories-first -l -s=modified'
alias lc='exa --git --group-directories-first --created -s=created'
alias lt='exa --git --group-directories-first --tree --git-ignore'
alias lr='exa --git --group-directories-first -R --git-ignore'

# shortcuts to edit various files
alias vim=nvim
alias ezsh='nvim ~/.zshrc'
alias esc='nvim ~/.oh-my-zsh/custom/scripts.sh'
alias rzsh='. ~/.zshrc'
alias val='nvim ~/.oh-my-zsh/custom/aliases.zsh'
alias ev='nvim ~/.config/nvim/init.vim'

# qol
alias wholistens='netstat -tulpn | rg LISTEN'
alias sf=screenfetch
alias duh='du -sh'
alias p=python3
alias c='clipcopy'
alias map='xargs -n1'
alias md='mkdir -p'
function take {
  mkdir $@ && cd $@
}

# npm
alias ni='npm install'
alias nin='npm init'
alias nu='npm update'
alias nis='npm i -S'
alias nid='npm i -D'
alias nig='npm i -g'
alias nr='npm run'
alias nrs='npm run start'
alias nrss='npm run start:staging'
alias nrb='npm run build'
alias nrt='npm run test'
alias nrc='npm run commit'

alias mp3='youtube-dl -x --embed-thumbnail --audio-format mp3'

function blob() {
  echo "$@" | sed "s|blob://|s3://|g" | xargs s3cmd -c ~/.s3cfg-blob | sed "s|s3://|blob://|g" | sed "s|s3cmd |blob |g";
}

function up() {
  curl -F"file=@$@" http://0x0.st
}

function open_command() {
  local open_cmd

  # define the open command
  case "$OSTYPE" in
    darwin*)  open_cmd='open' ;;
    cygwin*)  open_cmd='cygstart' ;;
    linux*)   [[ "$(uname -r)" != *icrosoft* ]] && open_cmd='nohup xdg-open' || {
                open_cmd='cmd.exe /c start ""'
                [[ -e "$1" ]] && { 1="$(wslpath -w "${1:a}")" || return 1 }
              } ;;
    msys*)    open_cmd='start ""' ;;
    *)        echo "Platform $OSTYPE not supported"
              return 1
              ;;
  esac

  ${=open_cmd} "$@" &>/dev/null
}

alias ytdl=youtube-dl

# more git aliases
alias gin='git init'
alias gcd='git checkout dev'
alias gdf='git diff FETCH_HEAD'
alias gcnv!='git commit --no-verify'

alias dc='docker-compose'
alias e2e='docker-compose -f ~/code/cardiologs/front/cypress/docker-compose.yml'
alias python3.6='python3'

# tmux
alias ta='tmux attach -t'
alias tad='tmux attach -d -t'
alias tkss='tmux kill-session -t'
alias tksv='tmux kill-server'
alias tl='tmux list-sessions'
alias ts='tmux new-session -s'

function pasters {
  local file=${1:-/dev/stdin}
  curl --data-binary @${file} https://paste.rs
}

function cardinal {
  /home/wadii/.virtualenvs/cardinal/bin/python3 -m cardinal $@
}

function gs() {
  if command -v fsays &> /dev/null ; then
    program="fsays"
  else
    program="cowsay"
  fi
  $program "It's gss, idiot"
}
