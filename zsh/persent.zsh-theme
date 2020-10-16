GIT_DIRTY_SYMBOL="*"
ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[green]%}("
ZSH_THEME_GIT_PROMPT_SUFFIX=")%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg_bold[yellow]%}$GIT_DIRTY_SYMBOL%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_BACKGROUND_JOB="%{$reset_color%}%B%F{yellow}%(1j.!.)%b%f%{$reset_color%}"
COOL_UNICODE="%%"

git_custom_status() {
  local cb=$(git_current_branch)
  if [ -n "$cb" ]; then
    echo "$ZSH_THEME_GIT_PROMPT_PREFIX$(git_current_branch)$ZSH_THEME_GIT_PROMPT_SUFFIX"
  fi
}

PROMPT='%{$fg[gray]%}%B$(shrink_path -f)%b%{$reset_color%}$(parse_git_dirty)$ZSH_BACKGROUND_JOB %{$fg_bold[magenta]%}$COOL_UNICODE%{$reset_color%} '
RPROMPT='$(git_custom_status)'
