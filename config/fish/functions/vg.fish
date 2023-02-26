function vg -d "Grep pattern and select file where it appears"
    rg --line-number . | fzf --delimiter : --nth 3.. --bind 'enter:become($EDITOR {1} +{2})'
end
