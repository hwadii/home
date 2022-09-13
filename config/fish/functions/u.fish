function u --description "Send file or stdin to https://paste.rs"
  set -l filepath $argv
  if test -e "$filepath"
      curl --data-binary @$filepath https://paste.rs
  else
      curl --data-binary @/dev/stdin https://paste.rs
  end
end
