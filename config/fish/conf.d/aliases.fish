# verbose
abbr rm rm -v
abbr cp cp -v
abbr mv mv -v

# list
abbr ls eza
abbr ll eza --git --group-directories-first --hyperlink -l
abbr lll eza --git --group-directories-first --hyperlink -l --icons -h
abbr la eza --git --group-directories-first --hyperlink -l -a -I.DS_Store
abbr lh eza --git --group-directories-first --hyperlink -l -a -H -I.DS_Store
abbr lm eza --git --group-directories-first --hyperlink -l -s=modified
abbr lc eza --git --group-directories-first --hyperlink --created -s=created
abbr lt eza --git --group-directories-first --hyperlink --tree --git-ignore
abbr lr eza --git --group-directories-first --hyperlink -R --git-ignore

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
abbr x aunpack
