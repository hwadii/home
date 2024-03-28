function take -d "Create directory and change into it" -a name
    mkdir -p "$name" && cd "$name"
end
