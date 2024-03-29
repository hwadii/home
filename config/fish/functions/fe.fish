function fe -d "Open the selected file with the default editor"
    set files (fd -tf --hidden | fzf --query="$argv[1]" --multi --select-1 --exit-0)
    test -n "$files" && eval "$EDITOR $files"
end
