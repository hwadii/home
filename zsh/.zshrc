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

source $XDG_CONFIG_HOME/zsh/persent.zsh-theme
source $XDG_CONFIG_HOME/zsh/quick-open.zsh
source $XDG_CONFIG_HOME/zsh/aliases.zsh
source $HOME/.zinit/bin/zinit.zsh

zinit for \
    light-mode  zdharma/fast-syntax-highlighting \
                zdharma/history-search-multi-word

zinit as"null" wait"3" from"gh-r" lucid for \
  light-mode \
    mv"fd* -> fd" @sharkdp/fd \
    junegunn/fzf \
    mv"exa* -> exa" ogham/exa

zinit ice wait lucid atload'_zsh_autosuggest_start'
zinit light zsh-users/zsh-autosuggestions
zinit ice from"gh-r" as"program" bpick="*amd64.rpm"
zinit light cli/cli
zinit snippet OMZ::lib/git.zsh
zinit snippet OMZ::plugins/git/git.plugin.zsh
zinit snippet OMZ::plugins/fancy-ctrl-z/fancy-ctrl-z.plugin.zsh
zinit snippet OMZ::lib/completion.zsh
zinit snippet OMZ::plugins/dnf/dnf.plugin.zsh
zinit snippet OMZ::plugins/extract/extract.plugin.zsh
zinit ice as"program"
zinit snippet https://github.com/junegunn/fzf/blob/master/bin/fzf-tmux
zinit blockf for \
  light-mode \
    zsh-users/zsh-completions \
    mv"completions.zsh -> _exa" ogham/exa

DISABLE_UNTRACKED_FILES_DIRTY="true"

bindkey "^Xa" _expand_alias
bindkey -e

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

bindkey "^R" history-search-multi-word
if [[ -n "${terminfo[kcbt]}" ]]; then
  bindkey -M emacs "${terminfo[kcbt]}" reverse-menu-complete
  bindkey -M viins "${terminfo[kcbt]}" reverse-menu-complete
  bindkey -M vicmd "${terminfo[kcbt]}" reverse-menu-complete
fi
compinit
