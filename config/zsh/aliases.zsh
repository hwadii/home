# verbose
alias rm='rm -v'
alias cp='cp -v'
alias mv='mv -v'
alias -s txt=cat

alias sard='mpv https://twitch.tv/sardoche --quiet >/dev/null &'
alias kameto='mpv https://www.twitch.tv/kamet0 --quiet >/dev/null &'
alias coro='mpv https://twitch.tv/corobizar --quiet >/dev/null &'
alias open='open_command'
alias dragon-x11="GDK_BACKEND=x11 dragon"

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

alias ytdl=yt-dlp

# more git aliases
alias gin='git init'
alias gcd='git checkout dev'
alias gdf='git diff FETCH_HEAD'
alias gcnv!='git commit --no-verify'

alias dc='docker compose'
alias e2e='docker compose -f ~/code/cardiologs/front/cypress/docker-compose.yml'

# tmux
alias ta='tmux attach -t'
alias tad='tmux attach -d -t'
alias tkss='tmux kill-session -t'
alias tksv='tmux kill-server'
alias tl='tmux list-sessions'
alias ts='tmux new-session -s'

# extract
alias x="aunpack --explain"
