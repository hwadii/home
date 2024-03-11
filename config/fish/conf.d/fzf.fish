set -x FZF_DEFAULT_OPTS_FILE ~/.config/fzf/fzfrc
set -x FZF_ALT_C_COMMAND fd --type d --hidden --follow --exclude .git --exclude node_modules
set -x FZF_ALT_C_OPTS "--preview='tree -C {}' | head -200"
set -x FZF_CTRL_T_COMMAND "fd --type file --hidden --follow --min-depth 1 --full-path \$dir"
set -x FZF_CTRL_T_OPTS "--select-1 --exit-0"
set -x FZF_DEFAULT_COMMAND "fd --type f --follow --hidden -E.git"
