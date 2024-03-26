# verbose
abbr rm rm -v
abbr cp cp -v
abbr mv mv -v

# list
abbr ls eza --git --group-directories-first --hyperlink
abbr l ls
abbr ll ls -l
abbr lll ls -l --icons -h
abbr la ls -l -a -I.DS_Store
abbr lh ls -l -a -H -I.DS_Store
abbr lm ls -l -s=modified
abbr lc ls --created -s=created
abbr lt ls --tree --git-ignore
abbr lr ls -R --git-ignore

# shortcuts to edit various files
abbr vim nvim
abbr vi nvim

# qol
abbr wholistens "netstat -tulpn | rg LISTEN"
abbr map xargs -n1
abbr md mkdir -p

# more git aliases
abbr gin git init
abbr gcnv! git commit --no-verify

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
