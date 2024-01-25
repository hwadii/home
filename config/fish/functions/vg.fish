function vg -d "Grep pattern and select file where it appears"
    set ret (rg --line-number --no-heading $argv | fzf -0 -1 --multi | awk -F: '{print $1, $2}' | string split " ")
    set file $ret[1]
    set line $ret[2]
    test -n "$file" && eval "$EDITOR $file +$line"
end
