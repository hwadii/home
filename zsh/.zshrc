export ZSH=$HOME/.oh-my-zsh 
export BIN=$HOME/bin
export PATH="$PATH:$HOME/.local/bin:$HOME/.config/nvm/versions/node/v14.3.0/bin/:$HOME/bin:$HOME/.rvm/bin"
export SUDO_PROMPT=$'\e[35m[sudo]\e[33m password for %p:\e[0m '
export XDG_CONFIG_HOME="$HOME/.config"
export EDITOR=nvim
export NVM_DIR="$HOME/.config/nvm"
export FZF_DEFAULT_COMMAND='rg --files --follow --hidden -g "!node_modules" -g "!.git"'
export FZF_DEFAULT_OPTS='--reverse --height 50% --inline-info'
export FZF_CTRL_T_OPTS="--select-1 --exit-0"
export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -200'"
export FZF_TMUX_OPTS="-p 50%"
export NVM_DIR="$HOME/.config/nvm"
export TERM="xterm-256color"

DISABLE_UNTRACKED_FILES_DIRTY="true"
ZSH_THEME="persent"
plugins=(git dnf tmux extract sprunge zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
