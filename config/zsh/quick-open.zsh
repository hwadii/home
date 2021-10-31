# fe [FUZZY PATTERN] - Open the selected file with the default editor
#   - Bypass fuzzy finder if there's only one match (--select-1)
#   - Exit if there's no match (--exit-0)
fe() (
  IFS=$'\n' files=($(fzf --query="$1" --multi --select-1 --exit-0 --preview-window '~1' --preview "bat --style header,changes --color=always --decorations=always {} | head -500"))
  [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
)

# Modified version where you can press
#   - CTRL-O to open with `open` command,
#   - CTRL-E or Enter key to open with the $EDITOR
fo() {
  set -e
  IFS=$'\n' out=("$(fzf --query="$1" --exit-0)")
  file=$(head -2 <<< "$out" | tail -1)
  if [ -n "$file" ]; then
    nohup xdg-open "$file" &>/dev/null
  fi
}

# fgit [FUZZY PATTERN] - Open the modified file with the default editor
fgit() (
  IFS=$'\n' files=($(git status --short | awk '{ print $2 }' | fzf --query="$1" --multi --select-1 --exit-0 --preview "bat --style numbers,changes --color=always --decorations=always {} | head -500"))
  [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
)

fh() {
    print -z $( ([ -n "$ZSH_NAME"  ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//' )
}

function open_command() {
  nohup xdg-open "$@" &>/dev/null
}
