skip_global_compinit=1
export LANG="en_US.UTF-8"
export SNAP="/snap"
export SWAYSOCK=$(find /run/user -name 'sway-ipc.*.sock' 2>/dev/null | head -n1)
export XDG_CURRENT_DESKTOP=sway
export MONITOR="eDP-1"
export EXTERN_HOME="DP-1"
export EXTERN_WORK="HDMI-2"
export BROWSER="firefox"
export LESS="--RAW-CONTROL-CHARS --mouse -Ri"
export XDG_CONFIG_HOME="$HOME/.config"
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/ripgreprc"
export BIN=$HOME/bin
export NVM_DIR="$HOME/.config/nvm"
export RVM_DIR="$HOME/.rvm"
export CARGO_HOME="$HOME/.local/cargo"
export RUSTUP_HOME="$HOME/.local/rustup"
export DEFAULT_PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin"
export GEM_HOME="$RVM_DIR/gems/ruby-2.7.2"
export PATH="$HOME/.local/bin:$BIN:$DEFAULT_PATH:$HOME/.config/nvm/versions/node/v15.3.0/bin:$HOME/.rvm/bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$BIN/git-extras:$HOME/.local/texlive/2020/bin/x86_64-linux:$CARGO_HOME/bin:$RVM_DIR/gems/ruby-2.7.2/bin:$RVM_DIR/rubies/default/bin"
export ZSH=$HOME/.oh-my-zsh
export SUDO_PROMPT=$'\e[35m[sudo]\e[33m password for %p:\e[0m '
export EDITOR=nvim
export FZF_DEFAULT_COMMAND='fd --type f --follow --hidden -E".git"'
export FZF_DEFAULT_OPTS="
    --height 40%
    --bind 'tab:down' --bind 'btab:up' --bind 'ctrl-s:toggle'
"
export FZF_CTRL_T_OPTS="--select-1 --exit-0"
export FZF_ALT_C_COMMAND="fd --type d --hidden --follow --exclude '.git' --exclude 'node_modules'"
export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -200'"
[[ -f ~/.npmrc ]] && export $(grep npm.cardiologs.com/:_authToken ~/.npmrc|awk -F \"  '{print "CDL_NPM_TOKEN="$2}')
