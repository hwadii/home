function peek -d "Unzip archive in tmp" -a filepath
    if not test -e "$filepath"
        echo "Pass file to peek"
        return 1
    end
    set work (mktemp -d)
    atool -X "$work" "$filepath"
    pushd "$work"
    echo "$filepath"
end
