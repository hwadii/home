function scr -d "Quickly create a tmp directory" -a "operation"
  switch $operation
  case new
    mkdir -p /tmp/workspaces
    set work (mktemp -p "/tmp/workspaces" -d)
    pushd "$work"
  case view
    mkdir -p /tmp/workspaces
    pushd /tmp/workspaces
    pushd (eza -snew | fzf --preview 'eza -A {}')
  case clean
    rm -rf /tmp/workspaces/*
  case '*'
    return 1
  end
end
