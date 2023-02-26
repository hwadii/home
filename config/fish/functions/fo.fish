function fo -d "Open the selected file with xdg-open"
    fd -tf | fzf --query="$argv[1]" --multi --exit-0 --bind 'enter:become(open {+})'
end
