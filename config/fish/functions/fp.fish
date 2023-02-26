function fp -d "Open the selected file with default editor and with preview"
    fzf --query="$1" --multi --select-1 --exit-0 --preview-window '~1' \
        --preview "bat --style header,changes --color=always --decorations=always {} | head -500" \
        --bind="enter:become($EDITOR {+})"
end
