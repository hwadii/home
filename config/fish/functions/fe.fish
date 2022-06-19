function fe -d "Open the selected file with the default editor"
    set files (fzf --query="$argv[1]" --multi --select-1 --exit-0)
    test -n "$files" && nvim $files
end
