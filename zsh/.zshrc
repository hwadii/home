export LANG="en_US.UTF-8"
export SNAP="/snap"
export SWAYSOCK=$(ls /run/user/*/sway-ipc.*.sock | head -n 1)
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
export PATH="$HOME/.local/bin:$BIN:$DEFAULT_PATH:$HOME/.config/nvm/versions/node/v14.15.1/bin:$HOME/.rvm/bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$BIN/git-extras:$HOME/.local/texlive/2020/bin/x86_64-linux:$CARGO_HOME/bin:$RVM_DIR/gems/ruby-2.7.2/bin:$RVM_DIR/rubies/default/bin"
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
export $(grep npm.cardiologs.com/:_authToken ~/.npmrc|awk -F \"  '{print "CDL_NPM_TOKEN="$2}')

DISABLE_UNTRACKED_FILES_DIRTY="true"
ZSH_THEME="persent"
plugins=(git gitfast dnf tmux extract zsh-syntax-highlighting fancy-ctrl-z)

source $ZSH/oh-my-zsh.sh

bindkey "^Xa" _expand_alias

copybuffer() {
  echo "$BUFFER" | wl-copy -n
}
zle -N copybuffer
bindkey "^O" copybuffer

_fzf_compgen_path() {
  fd --hidden --follow --exclude ".git" . "$1"
}

_fzf_compgen_dir() {
  fd --type d --hidden --follow --exclude ".git" . "$1"
}

eval "$(zoxide init zsh)"
[ -f $HOME/Documents/creds.zsh ] && source $HOME/Documents/creds.zsh
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f ~/code/git/git-extras/etc/git-extras-completion.zsh ] && source ~/code/git/git-extras/etc/git-extras-completion.zsh
