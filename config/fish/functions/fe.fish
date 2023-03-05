function fe -d "Open the selected file with the default editor"
    set files (fd -tf | fzf --query="$argv[1]" --multi --select-1 --exit-0)
    test -n "$files" && $EDITOR $files
end
