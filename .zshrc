export ZSH=$HOME/.oh-my-zsh 
export DL="$HOME/Téléchargements"
export ANDROID_HOME="/home/hwadii/cruft/sdk-tools/"
export ANDROID_SDK_ROOT="/home/hwadii/cruft/sdk-tools/"
export PATH="$PATH:/home/hwadii/cruft/sdk-tools/tools:/home/hwadii/cruft/sdk-tools/tools/bin:/home/hwadii/cruft/sdk-tools/platform-tools:$DL/genymotion/tools:/home/hwadii/.local/bin:$HOME/.nvm/versions/node/v13.1.0/bin"
export SUDO_PROMPT=$'\e[34m[sudo]\e[33m password for %p:\e[0m '
export XDG_CONFIG_HOME="$HOME/.config"
export EDITOR=nvim
export FZF_DEFAULT_COMMAND='rg --files --follow --hidden'

PROJECT_PATHS=(~/code ~/work)
ZSH_THEME="robbyrussell"
DISABLE_UNTRACKED_FILES_DIRTY="true"
plugins=(git dnf tmux transfer pj)

source $ZSH/oh-my-zsh.sh
source $ZSH/custom/scripts.sh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
