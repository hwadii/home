export XDG_CONFIG_HOME="$HOME/.config"
export ZSH=$HOME/.oh-my-zsh 
export BIN=$HOME/bin
export PATH="$PATH:$HOME/.local/bin:$HOME/.config/nvm/versions/node/v14.6.0/bin:$HOME/bin:$HOME/.rvm/bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$HOME/bin/git-extras"
export SUDO_PROMPT=$'\e[35m[sudo]\e[33m password for %p:\e[0m '
export EDITOR=nvim
export NVM_DIR="$HOME/.config/nvm"
export FZF_DEFAULT_COMMAND='rg --files --follow --hidden -g "!node_modules" -g "!.git"'
export FZF_DEFAULT_OPTS='--reverse --height 50% --inline-info'
export FZF_CTRL_T_OPTS="--select-1 --exit-0"
export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -200'"
export NVM_DIR="$XDG_CONFIG_HOME/nvm"
export TERM="xterm-256color"

DISABLE_UNTRACKED_FILES_DIRTY="true"
ZSH_THEME="persent"
plugins=(git dnf tmux extract sprunge zsh-syntax-highlighting wd)

source $ZSH/oh-my-zsh.sh

[ -f $HOME/Documents/creds.zsh ] && source $HOME/Documents/creds.zsh
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f ~/code/git/git-extras/etc/git-extras-completion.zsh ] && source ~/code/git/git-extras/etc/git-extras-completion.zsh
