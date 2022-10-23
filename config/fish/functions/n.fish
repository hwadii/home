function n --wraps nnn --description 'Support nnn quit and change directory'
    # Block nesting of nnn in subshells
    if test -n "$NNNLVL" -a "$NNNLVL" -ge 1
        echo "nnn is already running"
        return
    end

    set NNN_TMPFILE "$XDG_CONFIG_HOME/nnn/.lastd"

    # The command function allows one to alias this function to `nnn` without
    # making an infinitely recursive alias
    command nnn $argv

    if test -e $NNN_TMPFILE
        source $NNN_TMPFILE
        command rm $NNN_TMPFILE
    end
end
