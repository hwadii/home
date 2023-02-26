function fe -d "Open the selected file with the default editor"
    fd -tf | fzf --query="$argv[1]" --multi --exit-0 --bind 'enter:become($EDITOR {+})'
end
