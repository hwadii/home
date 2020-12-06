[ -z "$HISTFILE" ] && HISTFILE="$HOME/.zsh_history"
HISTSIZE=99999
SAVEHIST=90000
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history
setopt hist_find_no_dups
setopt hist_reduce_blanks
setopt extended_glob
setopt equals
setopt prompt_subst
setopt auto_cd
setopt interactivecomments
setopt auto_continue
setopt auto_param_slash
setopt pushd_ignore_dups
export DIRSTACKSIZE=20
setopt auto_pushd

zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' max-errors 3 numeric
function fancy_ctrl_z() {
  if [[ $#BUFFER -eq 0 ]]; then
    export BUFFER='fg'
    zle accept-line
  else
    zle push-input
    zle clear-screen
  fi
}
bindkey ' ' magic-space # history expansion
zle -N fancy_ctrl_z
bindkey '^Z' fancy_ctrl_z
autoload edit-command-line

autoload -Uz bracketed-paste-magic
zle -N bracketed-paste bracketed-paste-magic

zreload() {
  local cache="$ZSH_CACHE_DIR"
  autoload -U compinit zrecompile
  compinit -i -d "$cache/zcomp-$HOST"

  for f in ${ZDOTDIR:-~}/.zshrc "$cache/zcomp-$HOST"; do
    zrecompile -p $f && command rm -f $f.zwc.old
  done

  [[ -n "$SHELL" ]] && exec ${SHELL#-} || exec zsh
}

autoload -U compaudit compinit

source ~/.config/zsh/persent.zsh-theme
source ~/.config/zsh/quick-open.zsh
source ~/.config/zsh/aliases.zsh
source ~/.zinit/bin/zinit.zsh

zinit for \
    light-mode  zsh-users/zsh-autosuggestions \
                zdharma/fast-syntax-highlighting

zinit light zdharma/history-search-multi-word
zinit ice from"gh-r" as"program"
zinit light junegunn/fzf
zinit snippet OMZ::lib/git.zsh
zinit snippet OMZ::plugins/git/git.plugin.zsh
zinit snippet OMZ::lib/completion.zsh
zinit snippet OMZ::plugins/dnf/dnf.plugin.zsh
zinit snippet OMZ::plugins/tmux/tmux.plugin.zsh
zinit snippet OMZ::plugins/extract/extract.plugin.zsh
zinit light zsh-users/zsh-completions

DISABLE_UNTRACKED_FILES_DIRTY="true"

bindkey "^Xa" _expand_alias
bindkey -e

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

function mem() { ps -axv | grep $$  }

eval "$(zoxide init zsh)"
[ -f $HOME/Documents/creds.zsh ] && source $HOME/Documents/creds.zsh
[ -f ~/code/git/git-extras/etc/git-extras-completion.zsh ] && source ~/code/git/git-extras/etc/git-extras-completion.zsh

compinit
