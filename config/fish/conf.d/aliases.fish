# verbose
alias rm "rm -v"
alias cp "cp -v"
alias mv "mv -v"

# list
alias ls eza
alias l "eza -l"
alias ll "eza --git --group-directories-first --hyperlink -l"
alias la "eza --git --group-directories-first --hyperlink -lah"

# shortcuts to edit various files
abbr vim nvim
abbr vi nvim

# qol
abbr wholistens "netstat -tulpn | rg LISTEN"
abbr map xargs -n1
abbr md mkdir -p

# lazygit
abbr lg lazygit

abbr dc docker compose

# tmux
abbr ta tmux attach -t
abbr tad tmux attach -d -t
abbr tkss tmux kill-session -t
abbr tksv tmux kill-server
abbr tl tmux list-sessions
abbr ts tmux new-session -s

# extract
alias x aunpack

abbr fe file-edit
abbr fo file-open
abbr fp file-preview
