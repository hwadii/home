# general functions

function tw() (
  nohup mpv "https://twitch.tv/$@" --quiet > /dev/null 2>&1 &
)

function blob() (
  echo "$@" | sed "s|blob://|s3://|g" | xargs s3cmd -c ~/.s3cfg-blob | sed "s|s3://|blob://|g" | sed "s|s3cmd |blob |g";
)

function up() (
  curl -F"file=@$@" http://0x0.st
)

function open_command() (
  nohup xdg-open "$@" &>/dev/null
)


function pasters() (
  local file=${1:-/dev/stdin}
  curl --data-binary @${file} https://paste.rs
)

function cardinal() {
  /home/wadii/.virtualenvs/cardinal/bin/python3.8 -m cardinal $@
}

backport() { /home/wadii/.virtualenvs/backport/bin/backport $@ }

function gs() (
  local program
  if command -v fsays &> /dev/null ; then
    program="fsays"
  else
    program="cowsay"
  fi
  ${=program} "It's gss, idiot"
)

function take {
  mkdir $@ && cd $@
}

# fzf based functions

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
  IFS=$'\n' out=("$(fzf --query="$1" --exit-0 --expect=ctrl-o,ctrl-e)")
  key=$(head -1 <<< "$out")
  file=$(head -2 <<< "$out" | tail -1)
  if [ -n "$file" ]; then
    [ "$key" = ctrl-o ] && open "$file" || ${EDITOR:-vim} "$file"
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

vg() {
  local file
  local line

  read -r file line <<<"$(rg --line-number --no-heading $@ | fzf -0 -1 | awk -F: '{print $1, $2}')"

  if [[ -n $file ]]
  then
    ${EDITOR:-nvim} $file +$line
  fi
}
