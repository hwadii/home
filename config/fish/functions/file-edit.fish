function file-edit -d "Open the selected file with the default editor"
    argparse u/unrestricted -- $argv
    if test -z $_flag_u
        set files (fd -tf --hidden)
    else
        set files (fd -tf -u)
    end
    set -l selection (string split " " $files | fzf --query="$argv[1]" --multi --select-1 --exit-0)
    test -n "$selection" && eval "$EDITOR $selection"
    commandline -f repaint
end
