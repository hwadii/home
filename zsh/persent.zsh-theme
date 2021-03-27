GIT_DIRTY_SYMBOL="*"
ZSH_THEME_GIT_PROMPT_PREFIX="%U%F{green}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%f%u"
ZSH_THEME_GIT_PROMPT_DIRTY="%B%F{yellow}$GIT_DIRTY_SYMBOL%f%b"
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_BACKGROUND_JOB="%B%F{yellow}%(1j.!.)%b%f"
ZSH_PROMPT_CHAR=%B%(?.%F{yellow}.%F{red})»%f%b

git_custom_status() {
  local cb=$(git_current_branch)
  if [ -n "$cb" ]; then
    echo "$ZSH_THEME_GIT_PROMPT_PREFIX$(__git_ps1 %s)$ZSH_THEME_GIT_PROMPT_SUFFIX"
  fi
}

PROMPT='%F{gray}%B$(smart-pwd)%b%f$(parse_git_dirty)$ZSH_BACKGROUND_JOB $ZSH_PROMPT_CHAR%f '
RPROMPT='$(git_custom_status)'
