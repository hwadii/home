function lk --description "Open walk and cd into directory on exit"
  set loc (walk $argv); and cd $loc;
end
