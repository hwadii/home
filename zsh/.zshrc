export ZSH=$HOME/.oh-my-zsh 
export BIN=$HOME/bin
export PATH="$PATH:$HOME/.local/bin:$HOME/.config/nvm/versions/node/v13.12.0/bin/node:$HOME/bin:$HOME/.rbenv/bin:$HOME/.rbenv/versions/2.7.0/bin"
export SUDO_PROMPT=$'\e[34m[sudo]\e[33m password for %p:\e[0m '
export XDG_CONFIG_HOME="$HOME/.config"
export EDITOR=nvim
export FZF_DEFAULT_COMMAND='rg --files --follow --hidden -g "!node_modules" -g "!.git"'
export FZF_DEFAULT_OPTS='--reverse --height 50% --inline-info'
export FZF_CTRL_T_OPTS="--select-1 --exit-0"
export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -200'"
export NVM_DIR="$HOME/.config/nvm"
export LESS=-r

PROJECT_PATHS=(~/code ~/work)
ZSH_THEME="cloud"
DISABLE_UNTRACKED_FILES_DIRTY="true"
plugins=(git tmux extract)

source $ZSH/oh-my-zsh.sh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

