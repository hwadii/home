function fish_user_key_bindings
    bind --erase \ct
    bind --erase \cr
    bind --preset \el downcase-word
    bind \e\; fzf-file-widget
end
