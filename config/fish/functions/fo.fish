function fo -d "Open the selected file with xdg-open"
    set file (fd -tf | fzf --query="$argv[1]" --select-1 --exit-0)
    test -n "$file" && open $file
end
