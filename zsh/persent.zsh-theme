GIT_DIRTY_SYMBOL="!"
ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[green]%}("
ZSH_THEME_GIT_PROMPT_SUFFIX=")%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg_bold[yellow]%}$GIT_DIRTY_SYMBOL%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""
COOL_UNICODE="%%"

git_custom_status() {
  local cb=$(git_current_branch)
  if [ -n "$cb" ]; then
    echo "$ZSH_THEME_GIT_PROMPT_PREFIX$(git_current_branch)$ZSH_THEME_GIT_PROMPT_SUFFIX"
  fi
}

PROMPT='%{$fg[gray]%}%B%1~%b%{$reset_color%}$(parse_git_dirty) %{$fg_bold[magenta]%}$COOL_UNICODE%{$reset_color%} '
RPROMPT='$(git_custom_status)'
