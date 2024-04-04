function file-preview -d "Open the selected file with default editor and with preview"
    set files (
      fzf --query="$1" --multi --select-1 --exit-0 --preview-window '~1' --preview "bat --style header,changes --color=always --decorations=always {} | head -500"
    )
    test -n "$files" && nvim $files
end
